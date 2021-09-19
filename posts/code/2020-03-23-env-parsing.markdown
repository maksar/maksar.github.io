---
title: Parsing environment variables with reverse tests
tags: haskell, lens, polysemy, QuickCheck
series: ldap-bot
language: russian
---

Тема сегодняшней статьи – чтение конфигурационных значений из переменных окружения и связанные с этим процессом трудности. В небольших системах, где нет необходимости в полноценном конфигурационном файле, принято брать настройки из переменных окружения, это один из ключевых моментов [12 factor app](https://12factor.net/config) манифеста. Это надежный и относительно безопасный способ конфигурации, он отлично поддерживается всеми операционными системами, облачными платформами и средствами контейнеризации.

<!--more-->

<img src="/previews/env-parsing/logo.jpg" class="center" />

"Так а что сложного-то?" спросите вы, "в любом языке программирования есть для этого встроенные средства, сдобренные ни одним десятком библиотек, упрощающих этот процесс. Действительно, проблем с тем, чтобы прочитать значение переменной окружения нет. Но если подходить к задаче не системно, запрашивая значения переменных окружения там и тут в коде, трудности все-же начнутся. Такую программу будет сложно сопровождать, так как существует множество мест в коде системы, где идет обращение к одной и той же переменной окружения. Но самое важное – такую систему будет сложно тестировать – необходимо использовать дополнительные ухищрения для подмены значений переменных окружения в тестовом режиме работы. Трудностей, со временем, будет становиться все больше, так как с добавлением нового функционала вырастет и количество настроек.

Способ преодоления таких трудностей эволюционно-естественен – необходимо сконцентрировать работу с конфигурацией в одном месте, сделать процесс добавления новой настройки понятным, упростить доступ к настройкам в коде бизнес-логики.

## Постановка задачи

В `Haskell`, несмотря на всю его "строгость" и приверженность к математически чистым функциям, тоже можно обращаться к переменным окружения откуда угодно, но "тут так не принято"... Язык подталкивает программиста отказаться от идеи так делать, заставляя явно отказываться от "чистоты" функций и терять все связанные с этим свойством преимущества. В мире строго-типизированных языков "удобно" не читать "настройки" посреди кода с логикой, а читать их в начале исполнения программы, преобразовать во внутреннюю структуру данных (с адекватными типами вместо строк) и использовать явно передавая такую структуру или ее части в остальные "вычисления" оставляя их свободными от side-effect-ов.

Довольно философствований, show me the code, как говорится.

```haskell
data Config = Config
  { _ldapHost               :: Text
  , _ldapPort               :: PortNumber
  , _port                   :: Int
  , _verifyToken            :: Text
  , _pageToken              :: Text
  , _user                   :: Text
  , _password               :: Text
  , _activeUsersContainer   :: Dn
  , _projectGroupsContainer :: Dn
  , _projectGroupsOrgunits  :: NonEmpty Text
  }
  deriving (Eq, Show, Generic, Default)
```

`Config` – та самая структура данных с настройками, необходимыми для работы [Group Manager](/posts/projects/2020-02-07-ldap-bot) бота. В начале работы системы, эта структура заполняется значениями из переменных окружения.

```haskell
readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig =
  Config <$> lookupText "LDABOT_LDAP_HOST"
         <*> lookupNumber "LDABOT_LDAP_PORT"
         <*> lookupNumber "LDABOT_PORT"
         <*> lookupText "LDABOT_VERIFY_TOKEN"
         <*> lookupText "LDABOT_PAGE_TOKEN"
         <*> lookupText "LDABOT_USERNAME"
         <*> lookupText "LDABOT_PASSWORD"
         <*> (Dn <$> lookupText "LDABOT_USERS_CONTAINER")
         <*> (Dn <$> lookupText "LDABOT_GROUPS_CONTAINER")
         <*> (fromList . splitOn "," <$> lookupText "LDABOT_GROUPS_ORGUNITS")
```

Как ни странно, функция `readConfig` является "чистой", хотя вроде бы и обращается к внешнему миру (то есть имеет side-effect-ы). Почему это так и как работает – я расскажу в следующей статье про "алгебраические эффекты". А пока, еще немного деталей реализации:

```haskell
lookupText :: (Member Environment r, Member (Error Text) r) => Text -> Sem r Text
lookupText name = lookupEnv name >>= \case
    Nothing     -> throw $ unwords ["Please set", name, "environment variable."]
    Just string -> return $ pack string

lookupNumber :: (Read a, Member Environment r, Member (Error Text) r) => Text -> Sem r a
lookupNumber name = read . unpack <$> lookupText name
```

Функция `lookupText` обращается к операционной системе через `lookupEnv name` и анализирует результат. Если значения не оказалось – генерируется ошибка, в противном случае – функция возвращает значение переменной окружения. `lookupNumber` является надстройкой над `lookupText`, которая после успешного получения значения конвертирует его в число. Интересным моментом тут является оператор `<$>` (так же известный как `fmap` в `Haskell` или `Optional.map` в `Java`). Его использование позволяет "не засорять" код обработкой граничных случаев вида "если `lookupText` вернул `null`, то тоже вернуть `null`; в противном случае – преобразовать в число и вернуть". Если вы вспомнили про elvis-оператор, то знайте, он является лишь частным случаем `fmap` для `null`-ов ;)

`<$>` несколько раз применяется еще и внутри `readConfig` для тех же целей – преобразовывать прочитанное из `LDABOT_USERS_CONTAINER` в `Dn` (термин из мира `LDAP`, означает `distinguished name`) есть смысл только если там что-то было. Самое первое использование `<$>` немного интереснее. Помните рассказ про `<$>` из [первой статьи](/posts/code/2020-02-27-aeson-parsing) про парсинг `json`-а? Речь шла о том, чтобы "адаптировать" конструктор структуры данных `Message` (который принимает строки) к "парсеру строк". Если посмотреть на такую адаптацию с другой стороны – операция `<$>` превращала "парсер строк" в "парсер `Message`-ей" постулируя "когда (и если) оригинальный парсер строк что-нибудь вернет, примени к этому конструктор `Message`".

С `Config`-ом ситуация та же, оператор `<$>` постулирует "когда (и если) **все** операнды для вызова функции `Config` будут готовы – вызывай". Если ранее мы конструировали `Message` "в контексте" парсера, который может ничего "не напарсить", то сейчас мы конструируем `Config` "в контексте" вычисления, которое может вернуть ошибку. `fmap` – он как обычный `map`, только не для списков, а для любых "контейнеров" или "вычислений" (деревья, Optional, парсер, генератор). Подготовка операндов происходит при помощи `<*>`. Его отличие от `<$>` в том, что теперь с обоих сторон "вычисления, которые могут вернуть ошибку". Механика сложная, зато код элегантный, без постоянных проверок (привет программистам на `golang`) и early return-ов.

## Тестирование

С проблематикой вроде разобрались, пора начинать извлекать пользу. из "централизации" работы с настройками а так же от использования "чистых" функций (не зря же прилагались усилия). С точки зрения кода, читающего значения переменных – совершенно не важно откуда именно происходит чтение – из реальных переменных окружения или из заранее подготовленного ассоциативного массива, главное, чтобы `lookupEnv` возвращала `Maybe Text`. Определив "тестовое окружение" как простой писок ключ-значение `type EnvironmentMock = [(Text, Text)]`, можно заставить `readConfig` читать данные из заранее подготовленного места.

```haskell
withMockedEnvironment :: EnvironmentMock -> Sem '[Environment, Error Text] a -> Either Text a
withMockedEnvironment mockedEnv = run . runError . fakeEnvironment mockedEnv

fakeEnvironment :: Member (Error Text) r => EnvironmentMock -> InterpreterFor Environment r
fakeEnvironment mockedEnv = interpret $ \case
  LookupEnv name -> return $ unpack <$> lookup name mockedEnv

withMockedEnvironment
  [ ("LDABOT_LDAP_HOST", "host")
  , ("LDABOT_LDAP_PORT", "123")
  , ("LDABOT_PORT", "234")
  , ("LDABOT_VERIFY_TOKEN", "vtoken")
  , ("LDABOT_PAGE_TOKEN", "ptoken")
  , ("LDABOT_USERNAME", "user")
  , ("LDABOT_PASSWORD", "pass")
  , ("LDABOT_USERS_CONTAINER", "ucont")
  , ("LDABOT_GROUPS_CONTAINER", "gcont")
  , ("LDABOT_GROUPS_ORGUNITS", "ou1,ou2")
  ] readConfig `shouldBe` Right Config {
    _ldapHost = "host",
    _ldapPort = 123,
    _port = 234,
    _verifyToken = "vtoken",
    _pageToken = "ptoken",
    _user = "user",
    _password = "pass",
    _activeUsersContainer = Dn "ucont",
    _projectGroupsContainer = Dn "gcont",
    _projectGroupsOrgunits = "ou1" :| ["ou2"]
  }
```

Как говорит один мой знакомый, "мало вариативности". Он ярый поклонник разработки через тесты, (привет тебе, В.С.)". Ну что-ж, постараемся добавить вариативности и уважить скептиков, заявляющих при чтении таких тестов – "а как убедиться в том, что реализация не состоит из хардкода именно этих значений".

Есть такой прием в тестировании – проверять обратимость (`reverse(reverse(list)) === list`). Построение конфига из окружения - назовем прямым преобразованием Окружение -> Конфиг. Если бы у нас было обратное преобразование (из Конфига в Окружение, из которого такой Конфиг прочитан), то мы бы могли проверить, что применив сначала прямое преобрзование, а затем обратное – получается исходный Конфиг. Такую пару Окружения и Конфига называют изоморфной, а само преобразование – изоморфизмом. Как обычно бывает в математике – слово сложное, но за ним стоит простая идея ;)

Если сначала конфиг (абсолютно любой) преобразовать в набор пар ключ-значение, а потом из них попытаться "прочитать" конфиг обратно, то в итоге должны ведь получить исходный конфиг.

```haskell
toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock Config {..} =
  [ ("LDABOT_LDAP_HOST", unpack _ldapHost)
  , ("LDABOT_LDAP_PORT", show _ldapPort)
  , ("LDABOT_PORT", show _port)
  , ("LDABOT_VERIFY_TOKEN", unpack _verifyToken)
  , ("LDABOT_PAGE_TOKEN", unpack _pageToken)
  , ("LDABOT_USERNAME", unpack _user)
  , ("LDABOT_PASSWORD", unpack _password)
  , ("LDABOT_USERS_CONTAINER", fromDn _activeUsersContainer)
  , ("LDABOT_GROUPS_CONTAINER", fromDn _projectGroupsContainer)
  , ("LDABOT_GROUPS_ORGUNITS", unpack $ intercalate "," $ toList _projectGroupsOrgunits)]
  where
    fromDn (Dn dn) = unpack dn
```

Имея прямое и обратное преобразование, можно записать:

```haskell
it "reads config from complete environment" $ forAll $ \config ->
  withMockedEnvironment (toEnvironmentMock config) readConfig === Right config
```

Но это только success случай мы протестировали, пока не ясно как будет себя вести функция чтения конфига, если в переменных окружения будет отсутствовать одно из значений. Но погодите-ка – ведь у нас же есть способ получить окружение в виде списка ключ-значение. Достаточно только удалить из нее одну (случайную) строку и попытаться прочитать конфиг:

```haskell
it "fails to read a config from incomplete environment" $ forAll $ \config -> do
  shuffled <- shuffle $ toEnvironmentMock config
  let ((missingKey, _), incompleteMock) = (head shuffled, tail shuffled)
  return $ withMockedEnvironment incompleteMock readConfig === Left (unwords ["Please set", missingKey, "environment variable."])
```

Ну вот, кажется удалось свести задачу тестирования функции чтения конфигурации к формированию произвольных конфигов. Эта задача для `Haskell` довольно типична – использовать property-based тестирование на нем очень любят. Так как структура `Config` состоит из достаточно примитивных типов и оберток над ними, то "произвольность" можно обеспечить с помощью всего нескольких строк.

```haskell
makeArbitrary ''Config

instance Arbitrary Config where
  arbitrary = arbitraryConfig
  shrink = recursivelyShrink
```

Благодаря тому, что `Config` теперь "реализует" `Arbitrary`, можно создавать "генератор" конфигов – `Gen Config` при помощи функции `arbitrary` из класса.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

Попробуем в REPL-е сгенерировать что-нибудь случайное:

```haskell
sample (arbitrary :: Gen Config)

Config
  { _ldapHost = "\nIUZ\DELu\EMUG1\DEL\1002298\11790\DC3s\STX"
  , _ldapPort = 20
  , _port = -17
  , _verifyToken = "\SO\DLE9_1\NUL\210889\681130l\ENQ"
  , _pageToken = "q\r;h1\959827\&1~\703396P1~\837562\190001xjf"
  , _user = "\466790\&6\DC4j"
  , _password = "{H"
  , _activeUsersContainer = Dn ""
  , _projectGroupsContainer = Dn "U\ACK\616135\570186v\672268\571313"
  , _projectGroupsOrgunits = "\615852L$\598568\ESC6\fc" :| ["[h\DC4N[3pzk\b\SUB6\133277\14775"]
  }
```

Работает! Теперь полиморфная функция `forAll`, обладающая типом `forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property` может принимать на вход "генератор конфигов" и проверять `Property` (по сути, чуть-чуть более хитрый предикат, где вместо `==` используется `===`).

```sh
Env
  environment reading
    reads config from complete environment
      +++ OK, passed 100 tests.
    fails to read a config from incomplete environment
      +++ OK, passed 100 tests.
```

Строка `+++ OK, passed 100 tests.` говорит о том, что было сгенерировано 100 случайных `Config`-ов для проверки инварианта – конвертации "окружение" и обратно. Количество тестов всегда можно задать аргументом командной строки при запусте тестов.

```sh
$ stack test --test-arguments --qc-max-success=10000

Env
  environment reading
    reads config from complete environment
      +++ OK, passed 10000 tests.
    fails to read a config from incomplete environment
      +++ OK, passed 10000 tests.

Finished in 2.2859 seconds
2 examples, 0 failures
```

От каких "ошибок" защищают такие defensive (regression, golden) тесты? Например, если случайно переставить местами строки при построении конфига – тесты это отловят. Либо если попытаться захардкодить какое-нибудь одно значение на этапе построения конфига – тесты тоже просигнализируют с несовпадении значений (сгенерированное случайное значение будет отличаться от статического хардкода). Изменение названия переменных, из которых читаем конфиг, такой тест тоже "отловит", но отловит тут в кавычках, потому что такое падение теста не говорит о некорректности или неработоспособности программы, оно говорит лишь о том, что тесты нужно обновить, по сути "зашив" в процедуру генерации фейкового оружения новые названия переменных. В.С. непременно бы заметил еще на этапе написания тестов, что названия переменных повторяются и в реализации и в тестах – "не DRY", сказал бы он в code review комментарии...

## Суши с лупой

Для того, чтобы избавиться из повторений, будем использовать популярную в функциональном программировании вещь – линзы. Линза, если совсем просто ее представить, это такая сущность, которая совмещает в себе getter и setter. Ну как setter... программирование же функциональное, immutability везде, нет никаких setter-ов, есть только функции `Value -> Object -> Object`, которые не меняют `Object`, а возвращают новый.

В структуре данных `Config` не случайно свойства начинались с символа подчеркивания, этому есть причина: для каждого поля структуры, `Haskell` объявит одноименную функцию с сигнатурой, например `_ldapHost :: Config -> Text`. Если бы поле называлось `ldapHost`, то часто бы возникал конфликт имен при объявлении временных "переменных". Да и смотря на использование `ldapHost` в коде подсознательно думаешь о нем, как о значении, а не как о функции.

Эту конвенцию "эксплуатирует" библиотека `lens`, позволяющая одной строкой сгенерировать линзы для каждого из полей структуры.

```haskell
makeLenses ''Config

ldapHost :: Lens' Config Text
ldapPort :: Lens' Config PortNumber
...
```

Для чего вообще эти линзы удобны? Для работы со вложенным структурами данных в функциональном стиле. Имея список составных объектов.

```haskell
data Color = Color {_shade :: Text}
data Material = Material {_kind :: Text, _color :: Color}
data Player = Player {_name :: Text, _material :: Material}

makeLenses ''Color
makeLenses ''Material
makeLenses ''Player

let players = [Player "Bender" (Material "metal" (Color "shiny"))
              ,Player "Fry" (Material "meat" (Color "yellow"))
              ,Player "Leela" (Material "meat" (Color "purple"))]
```

Можно выполнять нетривиальные операции "вглубь" на immutable данных используя "композицию линз" через знакомый оператор `.`:

```haskell
view material.color.shade $ head players
"shiny"

map (view $ material.color.shade) players
["shiny","yellow","purple"]

map (over (material.color.shade) (append "super_")) players
[Player {_name = "Bender", _material = Material {_kind = "metal", _color = Color {_shade = "super_shiny"}}}
,Player {_name = "Fry", _material = Material {_kind = "meat", _color = Color {_shade = "super_yellow"}}}
,Player {_name = "Leela", _material = Material {_kind = "meat", _color = Color {_shade = "super_purple"}}}]
```

Последний пример особенно нагляден, если бы не линзы, пришлось бы писать что-то вроде:

```haskell
map (\player ->
    let material = _material player
        color = _color material
        shade = _shade color
    in player { _material = material { _color = color { _shade = append "super_" shade } } }
  ) players
```

Вернемся к нашей задачу из избавлению от дублирования. Объявим список пар ключ-линза – никто не запрещает так сделать, ведь линза, по сути, всего-лишь сложная функция, а функции в `Haskell` first-class значения:

```haskell
settings = [
  ("LDABOT_LDAP_HOST",        ldapHost),
  ("LDABOT_LDAP_PORT",        ldapPort . isoRead . packed),
  ("LDABOT_PORT",             port . isoRead . packed),
  ("LDABOT_VERIFY_TOKEN",     verifyToken),
  ("LDABOT_PAGE_TOKEN",       pageToken),
  ("LDABOT_USERNAME",         user),
  ("LDABOT_PASSWORD",         password),
  ("LDABOT_USERS_CONTAINER",  activeUsersContainer . isoDn),
  ("LDABOT_GROUPS_CONTAINER", projectGroupsContainer . isoDn),
  ("LDABOT_GROUPS_ORGUNITS",  projectGroupsOrgunits . isoNonEmpty . splitted)]
  where
    isoRead :: (Read a, Show a) => Iso' a String
    isoRead     = iso show read
    isoDn       = iso (\(Dn dn) -> dn) Dn
    isoNonEmpty = iso toList fromList
    splitted    = iso (intercalate ",") (splitOn ",")
```

Обратите внимание на уже знакомые нам изоморфизмы снизу – пары функций, которые необходимы для преобразования линз к одному виду `Lens' Config Text`. Ведь исходя из типа `Config` линза `activeUsersContainer` работает с типом `Dn`, а мы хотим унифицировать все лизны в `settings` приведя их к одной, строковой сигнатуре.

Процедуру "чтения конфигурации" поменяем на свертку

```haskell
readConfig :: (Member Environment r, Member (Error Text) r) => Sem r Config
readConfig = foldM reducer (Config {}) settings
  where
    reducer config (name, lens) = do
      value <- lookup name
      return $ set lens value config
```

Код осуществляет свертку `foldM` при помощи функции `reducer` списка `settings`, используя в качестве начального значения пустой `Config {}`. Функция `reducer` имеет на входе два параметра – `config` в качестве аккумулятор-а и пара ключ-линза из списка `settings`. Она читает (`lookup name`) значение переменной окружения, устанавливает прочитанное значение при помощи линзы в `config` и возвращает его. Таким образом, последовательно пройдясь по всему списку `settings` все поля структуры `Config` окажутся заполнены значениями.

Наконец-то мы можем избавиться от дублирования названий переменных в тестах. Вместо свертки, делаем простой обход списка `map` просматривая через линзу значения в `config`-е.

```haskell
toEnvironmentMock :: Config -> EnvironmentMock
toEnvironmentMock config = map (\(name, lens) -> (name, view lens config)) settings
```

Использование инверсии, идемпотентности и других инвариантов - здорово помогает при написании тестов, Вариативность, как говорит мой знакомый - при этом "на высоте" ;)
