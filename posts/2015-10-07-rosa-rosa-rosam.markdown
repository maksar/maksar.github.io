---
title: Extensible effects
tags: russian, haskell, polysemy
---

С большим опозданием, продолжаю цикл статей про [WP бота](https://itransition.workplace.com/chat/t/105678357661487). Ссылки на предыдущие части: [1](2015-11-28-carpe-diem.html), [2](https://itransition.workplace.com/groups/143641062970056/permalink/486660242001468/) [3](https://itransition.workplace.com/groups/143641062970056/permalink/491875731479919/) и [4](https://itransition.workplace.com/groups/1394518670594144/permalink/3170796096299717/). Сегодня, впрочем как обычно, речь пойдет про очедную функциональную дичь ;).

Столпом ООП является инкапсуляция (каждый раз тянет по английски это слово с i начать), которая про "сокрытие реализации". Даже самому начинающему программисту известно, что достигается инкапсуляция в mainstream языках программирования при помощи interface-ов. Я постараюсь показать совершенно иной способ инкапсуляции – экзистенциальные эффекты.

Что за абстракция такая – "эффект" и для чего нужна? Для понимания, давайте рассмотрим примитивную функцию для сложения двух чисел:

```js
function add(a: Int, b: Int): Int {
  Logger.debug("${Time.current} - Adding numbers $a and $b.")
  return a + b;
}
```

Обычная функция – делает что и должна, да еще и записи в лог добавляет. Красота? С обывательской точки зрения с функцией все в порядке (возможные переполнения `Int` оставим за кадром). А вот с точки зрения компилятора – сплошное расстройство: вызов `add(2, 2)` невозможно вычислить и заменить на `4` на этапе сборки. Примерно так же на эту функцию смотрит и разработчик на Haskell – она просто "не может быть функцией". Точнее для него – это и не функция вовсе (в математическом смысле) – она не просто возвращает результат, совершая операции над аргументами. Она еще записывает сообщение в лог, должна откуда-то взять текущее время. Вызывая ее в разное время она хоть и вернет один и тот же результат, но в логи будет записаны разные строки. Обычно в таких случаях говорят, что у функции есть "побочный эффект" (side effect).

Наличие side effect-ов в императивных ЯП – обычное дело. Ими удобно пользоваться не только чтобы логи писать, но и для других, не менее Эффектных вещей – `Thread.current`, `Time.now`, `println` в конце концов. Проблема в том, что за удобство приходится расплачиваться. Высока ли цена? В этом примере – не очень. Подумаешь, в логи будут сыпаться сообщения при запуске unit тестов, а сама функция будет выполняться в 1000 раз медленнее чем могла бы... Если это и станет проблемой – мы либо вставим проверку `if (debug)` либо подменим `Logger` на "ничего не делающую заглушку" при запуске тестов. Один вопрос – а как мы узнаем о том, что это нужно сделать, глядя на сигнатуру функции? В том-то и дело что "никак"... Наличие side effect-а никак не отражено в сигнатуре типа, но тем не менее добавляет в код несколько неявных зависимостей – работу с логами и чтение времени.

В ООП с проблемой принято бороться только одним способом (хоть он и может выглядеть по-разному на первый взгляд) – при помощи техники Dependency Injection.

```js
class Calculator {
  @Autowired
  val logger: Logger;

  function add(a: Int, b: Int, calendar: Calendar = Time): Int {
    logger.debug("${calendar.current} - Adding numbers $a and $b.")
    return a + b;
  }
}
```

В такую функцию действительно можно передать и "пустой" логгер и "константный, замороженный во времени календарь" и даже протестировать поведение side effect-а, ожидая что у логгера вызовется функция `debug` с аргументом-строкой, начинающейся на то, что вернет `current` из переданного календаря. Это ведь был всего-лишь примитивный пример, в реальных системах количество таких неявных зависимостей явно больше пальцев на прямых руках. Да и если подходить скурпулезно, то и до [FizzBuzzEnterpriseEdition](https://github.com/EnterpriseQualityCoding/FizzBuzzEnterpriseEdition) недалеко...

Впрочем, в функциональном Haskell тоже можно писать примерно так:

```haskell
add :: Int -> Int -> IO Int
add a b = do
  getCurrentTime >>= \time -> putStrLn(time <> " - Adding numbers" <> intercalate " and " [a, b])
  return $ a + b
```

Обратите внимание, возвращаемый тип теперь не `Int`, а `IO Int`. Это "позволяет" внутри функции делать *что угодно* – хоть с лог писать, хоть по HTTP запросы слать. Работать с такой функцией все еще можно, но только из кода, который тоже "помечен" `IO`. Позволяя делать что угодно, `IO` как вирус заражает части программы, мешая пользоваться всеми преимуществами функционального подхода. Хотелось бы явно указывать – `add`, в качестве side effect-ов делает не *что угодно*, а только логгирование и работу со временем. Суть "эффектов" в этом и заключается – определить явно какие-то операции (влияющие или зависящие от "внешнего мира") – обозвать их эффектами и использовать в сигнатуре функций. В этом случае можно будет совершенно четко видеть с какими именно эффектами функция работает (и ничего другого ей позволено не будет).

Тело функции никак не меняется, другой становится сигнатура типа:

```haskell
add :: (Member TimeEffect eff, Member ConsoleEffect eff) => Int -> Int -> Effect eff Int
```

Читать ее можно так: допустим в композитном эффекте `eff` содержатся два эффекта – `TimeEffect` и `ConsoleEffect`, тогда функция `add` – принимая два `Int`-а возвращает `Int`, но при этом может выполнять "работу со временем" и "печатать на консоль". Посылать HTTP запросы ей не позволено, так как ни `TimeEffect` ни `ConsoleEffect` этого не позволяют. Их тоже определяет сам программист, например так:

```haskell
data TimeEffect m a where
  СurrentTime :: TimeEffect m String

makeEffect ''TimeEffect

data ConsoleEffect m a where
  PrintLn :: String -> ConsoleEffect m ()
  ReadLn :: ConsoleEffect m String

makeEffect ''ConsoleEffect
```

Ну хорошо, вместо всепозволяющего `Int -> Int -> IO Int` мы теперь имеем более конкретизированное `(Member TimeEffect eff, Member ConsoleEffect eff) => Int -> Int -> Effect eff Int` (обратите внимание на сходство с возможной математической нотацией: `add :: Int -> Int -> Effect eff Int`, где `eff` такое, что истинны оба утверждения: `Member TimeEffect eff` и `Member ConsoleEffect eff`).

Комбинируя функции с явно определенными эффектами в одной программе (функции) – эффекты тоже объединяются:

```haskell
greeting :: Member ConsoleEffect eff => Effect eff ()
greeting = do
  printLn("What is your name?")
  name <- readLn
  printLn("Hello" <> name)

measure :: (Memeber ConsoleEffect actionEff, Memeber TimeEffect measureEff) => Effect actionEff a -> Effect (measureEff : actionEff) a
measure action = do
  startTime <- currentTime
  result <- action
  endTime <- currentTime

  printLn $ "Action took: " <> endTime - startTime <> " seconds"
  return result

measure greeting :: (Memeber ConsoleEffect eff, Memeber TimeEffect eff) => Effect eff ()
```

По сути, мы создаем не последовательность инструкций, а сложную структуру данных – дерево последовательности вызовов и callback-ов. Дело за малым – интерпретировать (схлопнуть, вычислить) это дерево до получения единственного значения. Прелесть подхода с эффектами в том, что самостоятельно интерпретатор для дерева писать не приходится. Нужно всего-лишь "объяснить" что делать с тем или иным эффектом.

```haskell
runConsole :: Member (Embed IO) r => InterpreterFor ConsoleEffect r
runConsole = interpret $ \case
  PrintLn string -> System.IO.putStrLn string
  ReadLn -> System.IO.getLine

runTime :: Member (Embed IO) r => InterpreterFor TimeEffect r
runTime = interpret $ \case
  СurrentTime -> Data.Time.Clock.getCurrentTime
```

Итоговый интерпретатор, которым можно выполнить всю программу (именно он будет использоваться в main), комбинируют из индивидуальных:

```haskell
interpreter = runIO . runTime . runConsole
```

Применив получившийся интерпретатор `interpreter` к программе `measure greeting`, получим:

```js
=> What is your name?
<= Alex
=> Hello Alex
=> Action took: 5 seconds
```

Для целей тестирования функции `greeting` можно написать специальный интерпретатор, который по `readLn` всегда возвращает нужную нам строку, а `printLn` постоянно добавляет к аккумулятору переданную ему строку. Но создавать такие интерпретаторы – на наш путь. Мы воспользуемся готовеньким и реализуем эффект `ConsoleEffect` в терминах двух других библиотечных эффектов – `Reader` и `Writer`. В этом случае можно будет воспользоваться уже готовыми интерпретаторами для них:

```haskell
fakeInterpreter :: Effect ConsoleEffect a -> Effect (Writer String : Reader String) a
fakeInterpreter = reinterpret \case
  PrintLn string -> write string
  ReadLn -> ask
```

Была программа с side effect-ом `ConsoleEffect`, а стала программой для работы с эффектами `Reader String` и `Writer String` (про то почему reader и writer эффекты – можно и отдельную статью сделать). Выполнить ее можно получившимся "чистым" (без всяких `IO` и `Effect`) интерпретатором:

```haskell
pureGreeter :: String -> [String]
pureGreeter name = intepret greeter
  where
    intepret = run . runWriter . runReader name . fakeInterpreter
```

Тестировать такую функцию элементарно – передаешь имя на вход, получаешь массив строк на выходе – сравниваешь с ожидаемым.

Программирование "на эффектах" – во многом схоже с программированием "на интерфейсах" из объектно-ориентированного программирования. Выделяются абстракции, которые можно в последствии подменить (в целях тестирования или другого полиморфизма). В обоих случаях ядро программы представляет собой ответ на "что делать", а на "как делать" отвечает самая внешняя часть – чем ближе к main, тем [лучше](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell). В случае ООП этим занимается DI фреймворк – регистрирует при старте программы все реализации интерфейсов, внедряет зависимости туда, где они требуются и запускает исполнение основной программы.

Вроде все "так же", зачем тогда весь этот функциональные приседания? Да, на первый взгляд отличий не так много. Но задумайтесь – ваша программа перестала быть просто программой, она стала данными, выстроенным в памяти деревом вызовов, сформулированным в терминах выбранных вами эффектов. Это ведь можно как-то использовать...

Например для интроспекции – можно пройтись по по этой структуре и чего-нибудь туда добавить – трассировку, скажем:

```haskell
logConsole :: Member ConsoleEffect eff => Effect eff a -> Effect (eff : Trace) a
logConsole = intercept $ \case
  PrintLn string -> do
    trace $ unwords ["Going to print", length string, "characters"]
    printLn string
  ReadLn -> do
    trace $ unwords ["Going to read from a console"]
    result <- readLn
    trace $ unwords ["Successfully read", length result, "from a console"]
    return result
```

Или время заморозить, как в мультике:

```haskell
fronzen :: Member TimeEffect eff => Time -> Effect eff a -> Effect (eff : Trace) a
fronzen instant = intercept $ const $ pure instant
```

Кстати, а вы заметили в примерах множество полей с навешанными на них Autowired аннотациями или может быть десятки аргументов в конструкторах классов? Прелесть в том, что они оказываются не нужны – роль DI framework-а играет простая [композиция функций](https://git.itransition.com/projects/IA/repos/ldap-bot/browse/src/Server/Hook.hs#32). Чуть не забыл про [тесты]() упомянуть – благодаря тому, что все тестовые интерпретаторы не работают с `IO` – [выполняются](https://asciinema.org/a/389000) они довольно быстро:

```js
Running 1 test suites...
Test suite ldap-bot-test: RUNNING...

Finished in 0.0266 seconds
66 examples, 0 failures
```
