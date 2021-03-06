---
title: Developers' complaints
language: russian
---

Совсем недавно мы работали над списком улучшений для одного из проектов, которые собирались "предлагать" заказчику. В качестве предпосылки, имелся список замечаний от команды разработчиков. Есть мнение, что чистого "недовольства" проектом не бывает – это скорее кумулятивное понятие, которое либо озвучивают "только вслух" либо оформляют в виде "улучшений". Попробуем разобраться в истинных причинах недовольства, рассмотрев список улучшений "от разработчиков". Постараемся взглянуть на "недовольства" с двух сторон – со стороны разработчиков и со стороны менеджмента и/или заказчика. Ведь часто случается так, что одно и тоже выглядит совершенно по-разному.

<!--more-->

## Первая группа замечаний:

- ведут разработку на AWS dev серверах
- нет локально настроенного окружения и не планируют его делать

Касается "удобства" и "чувства продуктивности". Инженерам критически важно иметь "быстрый отклик" или "короткий feedback cycle" в процессе работы. При необходимости "ждать" (либо из-за медленного соединения c сетью, либо из-за необходимости синхронизации файлов и/или данных) растет раздражение и формируется недовольство. Теряется чувство "я продуктивен", нарастает недовольство собой как инженером, эффективно решающим задачу. Исходя из этого, разработчики часто отказываются работать на "удаленных" машинах, с подключением по RDP или TeamViewer. Хотя с точки зрения менеджмента – проблемы нет. Для управленца такой способ работы неотличим от "подумаешь, чуть более медленный компьютер, ничего страшного". Постоянные задержки приводят к отвлечению/переключению внимания инженера, что влияет не только на скорость выполнения задач (исправления багов, доработки функционала), но и на количество допускаемых в процессе ошибок. Причем совершенно непропорционально: необходимость работы "через RDP", который с точки зрения менеджмента медленнее локального окружения на 30% вполне может приводить к 3-х кратному увеличению сроков выполнения задач. Понимая это, менеджмент должен прилагать все возможные усилия к тому, чтобы снизить количество отвлечений инженеров, в том числе из-за отсутствия локального окружения, недостаточной производительности рабочего оборудования или скорости сетевого соединения.

## Вторая группа замечаний:

- неподдерживаемая версия языка
- неподдерживаемый/устаревший фрeймворк
- дублирующийся, закоментированный код
- отсутствие единого стиля кодирования

Говорит о другом виде недовольства – необходимости работы с большим количеством legacy кода, устаревшими подходами и технологиями. Инженерам по разработке программного обеспечения свойственна тяга к постоянному обучению, обновлению собственных знаний и умений. И это отнюдь не следствие "склада ума", а банальная необходимость. Если постоянно не стараться быть "на острие" технологий, версий компиляторов и языков – знания быстро устаревают и, как следствие, стоимость "на рынке" падает. Инженерам вовсе не хочется стать "неприменимым" на очередном собеседований из-за того, что последние несколько лет он/она трудились на проекте с не совсем современным технологическим стэком. С точки заказчика, несвоевременное обновление/поддержание с современном состоянии технологического стека может привести ко все более тяжелому и долгому поиску инженеров для работы над проектом. Более того, качество нанимаемых инженеров, согласных работать над таким проектом окажется ниже (количество предложений на рынке труда, пусть даже в рамках одной компании, велико). Таким образом, "модернизация" прежде всего выгодна заказчику и менеджменту, хоть на первый взгляд и кажется, что продиктована она лишь "недовольством" инженерного состава проекта.

## Третья группа замечаний:

- не используют менеджер пакетов
- JS зависимости в репозитории
- только часть стилей/изображений расположена в git-репозитории
- другая часть подключается как mounted-раздел с другого серверах

Относится к трудностям в области "управления зависимостями". Эти трудности схожи как с недовольством legacy кодом (отсутствует очевидный и быстрый способ "обновить" зависимость) так и непредвиденными задержками в разработке (вместо того, чтобы "обновить", приходится заниматься "IT археологией" и пытаться встроить fix в старую версию зависимости, тем самым еще более затрудняя ее будущее обновление). Постоянное ощущение "латания дыр" отрицательно сказывается на мотивации инженеров, не позволяет им сфокусироваться на важных задачах, спотыкаясь на "используемая у нас старая версия библиотеки этого не поддерживает". Причина, по которой своевременное обновление версий библиотек (языков, фреймворком, движков баз данных) "нужно" менеджменту и владельцу продукта отнюдь не в том, чтобы инженеры "чувствовали новизну". Причина в том, чтобы пользоваться возможностями новых версий, не тратя драгоценное время на back-port-инг функций или латание уязвимостей безопасности. Правильное управление зависимостями позволяет пользоваться наработками других, значительно сокращая время на разработку и поддержку продукта; снижать количество ошибок, возникающих из-за несовместимости версий и т.д.

## Опасения

Логичной реакцией заказчика (владельца продукта/системы) на предложения вида "давайте обновим, модернизируем, поменяем" являются опасения. Опасения того, изменение сложившегося status quo приведет к ошибкам в ПО, частичной неработоспособности, необходимости менять устоявшиеся процессы. Бизнес стремится быть консервативным (для противодействия этому эффекту даже существует отдельная должность – "директор по развитию бизнеса") – с точки зрения business continuity – совершенно логичное решение. Однако долгий период стабильности в IT сфере – отнюдь не тоже самое что в, скажем, мебельном бизнесе. Модернизация для сохранения темпа работ необходима на постоянной основе, а не в виде закупки нового деревообрабатывающего оборудования раз в 10 лет. К сожалению, скорость "устаревания" в IT много выше, необходимо отыскивать компромисс между стремлением инженерного состава к инновациям и требованиям бизнеса по сохранению стабильности.

Разумным считается выработка плана действий, который бы принимал во внимание опасения бизнеса, особенно в том случае, когда программный продукт уже является несколько устаревшим или разрабатывается при помощи/на базе устаревших технологий/библиотек/фреймворков. Ключевых рекомендаций для сохранения стабильности несколько:

- строгое отделение "улучшательств" от "поступательного развития"
- наличие плана Б в любой ситуации
- итеративный, эволюционный подход к изменениям

К примеру, естественным желанием технического лидера проекта на "неконсистентность форматирования" является действие "а давайте все унифицируем". Суть в том, чтобы не поддаваться соблазну "натравить linter на всю кодовую базу и вкомитить", пусть даже и кажется что это наименее инвазивным подходом (других разработчиков такое резкое изменение затронет в меньшей степени, ввиду его однократности) – это затруднит поиск авторов и причин написания того или иного участка кода (при поиске и решении каких-либо проблем). Ошибкой будет и внедрение политики "каждый применяет _правильное_ форматирование к файлам, которых касается" – это приведет к тому, что будет невозможно отличить изменения кода "по сути" задачи от изменений "из-за форматирования". Разумным компромиссом будет как раз гибридный подход, когда в рамках работы над задачей инженер все же делает изменения по "унификации форматирования", но намеренно отделяет их от основных изменений, связанных с решаемой в данный момент задачей. В дополнение к этому, стоит помечать коммиты с linting-ом особым образом, чтобы любому члену команды при просмотре истории было очевидно – этот коммит не содержит никаких изменений поведения.

Другим примером может служить обновление версии платформы (обновление версии языка и компилятора или платформы). Так как обычно проект не изобилует unit и интеграционными тестами, проверить работоспособность и корректность поведения после обновления – практически сложно. Ни программисты ни инженеры по контролю качества не способны дать гарантию того, что поведение системы не изменится. Соблюсти баланс между стремлением бизнеса к стабильности и необходимости модернизации в этом случае можно применив комбинацию эволюционного подхода и наличию запасного плана. Можно создать копию production окружения, провести обновление на нем и направить на него лишь небольшую часть траффика пользователей. Отслеживание возникающих ошибок (точнее их отсутсвие) за разумный промежуток времени послужит для бизнеса реальным свидетельством безопасности обновления. В случае же возникновения проблем, последствия окажутся во первых не велики (с новой версией работала небольшая часть пользователей), а во вторых – контролируемыми и предсказуемыми (инженеры могут заранее предопределить круг пользователей, направляемых на обновленную версию).

## Решение

Каждое конкретное изменение, предлагаемое инженерами, следует рассматривать через призму определенного "недовольства", которая может находить (или не находить) отражение в потребностях бизнеса. Конкретный план по улучшению определенного аспекта системы всегда должен формироваться в том числе исходя из потребности бизнеса в стабильности. Именно исходя из некоторого противоречия в стремлениях инженеров и бизнесменов рождается "здоровые" программные системы, которые программистам приятно поддерживать и развивать, а бизнесу – прогнозировать развитие и быть уверенным в надежности.
Однобокость в принятии решений как раз и приводит к перекосу: потакание желанию инженеров бездумно "использовать все новое и современное" легко может привести к тому, что продукт так и не выйдет на рынок и никогда не начнет приносить коммерческой выгоды. Равно как и стремление "оставить все как есть" неизбежно приведет к застою и устаревания, что, в свою очередь, сильно затруднит как talent aquisition так и технологичность продукта в целом.

Исходя из всего вышеизложенного, рекомендация заключается в том, чтобы по каждому из пунктов списка улучшений предлагаемых разработчиками иметь:

- формальную оценку важности (с точки зрения impact-а на систему)
- оценку сложности внедрения изменения (не в виде часов, а в виде возможных рисков с их перечислением)
- перечень "выгод" для системы, инженеров и бизнеса
- разработанный план итеративного внедрения и возможностей по "тестированию" и откату изменений

Оформленные в таком виде proposal-ы никогда не стоит отвергать полностью, а лишь приоритизировать на основании мнения владельцев продукта (сформированного не только на основании "суждения" инженера, но на базе измеримых критериев и рисков, описанных в proposal-е). Работу над внедрением изменений можно планировать в либо в периоды "малой активности" либо другим подходящим к темпу проекта способом (не больше одного изменения в sprint-е). Совокупность всех описанных шагов можно назвать работой с техническим долгом (который на самом деле является техническим кредитом или займом, сложные проценты дают о себе знать...), необходимость которой индустрия давно не подвергает сомнению.