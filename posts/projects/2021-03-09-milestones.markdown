---
title: Power Automate for low-code services
tags: russian, Office 365, kotlin
---

Хочу рассказать о Low Code Platform, которой пришлось воспользоваться буквально на днях – Power Automate от Microsoft. Мне, как разработчику ПО, никогда не нравились подобные системы для "программирования мышкой". Казалось, что с их помощью сложно добиться хоть сколько-нибудь приемлемого результата. Причин несколько:

* бесишься из-за неудобности и глючности "интефрейса" – хочется бросить и переключиться в любимый текстовый редактор и написать там "пару строчек кода" вместо накликивания условий и выбора элементов из списка
* сетуешь на ограничения платформы, из-за невозможности смоделировать простейшие операции: нельзя "обновить" значение переменной используя текущее значение, такая вот борьба с рекурсией
* удивляешься тому как тебе придется поддреживать настроенный "бизнес процесс" – буквально смотря на его блок-схему, отвыкли от такого программисты еще со школьных времен.

Прорвавшись через начальную пелену отрицания, удалось посмотреть на все с другой стороны – постойте-ка, это же Low Code Platform – она и не предназначена для использования программистами... Все вдруг становится на свои места, если представить себя бизнес-аналитиком или менеджером. Суть алгоритмов отраженных в визуализированном виде – проста и понятна, знаний программирования для моделирования бизнес процессов не требуется, везде подсказки и автодополнение – красота!

Рассмотрим три случая использования – один без кода совсем, два остальных – с небольшим его количеством.

## Автоматизация создания проектных карт

Решение "без кода" автоматизирует процесс сбора данных для создания новой проектной карты. Для этого в Microsoft Forms создан [опросник](https://forms.office.com/Pages/ResponsePage.aspx?id=y5lyFjrnCEylee896o1YNaHtV6z8fXhPi05De-ZazKFUMUNROUU5TUdFWDFUQ0k1VlQzWlE1MTlPOCQlQCN0PWcu), подобный тому что используется в ежегодном корпоративном опросе. Power Automate реагирует на заполнение формы и запускает первый сценарий, в процессе которого из полей формы формируется <a href="/images/milestones/12.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">письмо</a>. Power Automate услужливо подсказывает, что есть переменные "Responders' email" (так как в форме мы указали, что заполнять ее могут тоьлко сотрудники организации), позволяет <a href="/images/milestones/13.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">накликивать</a> другие поля из формы (причем редактирование полей в форме не ломает сценарий – на самом деле используются GUID-ы). Если сильно хочется – можно и <a href="/images/milestones/6.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">выраженьице</a> составить несложное – с таким справится человек и без навыков программирования.

Следующий <a href="/images/milestones/4.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">сценарий</a> реагирует на отосланное письмо и <a href="/images/milestones/11.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">создает</a> задачи со встречами. Решение не концентрировать все шаги в одном сценарии осознанное - хочется, чтобы бизнес процесс мог начаться не только с заполнения формы, но и с получения письма от живого человека.
Людям без навыков программирования обязательно понравятся возможности отладки – на каждом шаге <a href="/images/milestones/18.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">видно</a> данные на входе и выходе, время работы и даже циклы удобно <a href="/images/milestones/3.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">пролистывать</a>.

## Обновление размеров проектов

Второе и третье использование Power Automate тоже касается проектных карт.

В ходе работы CTO/PMO Office, мы мониторим состояние проектов. Хочется концентрировать больше внимания на стартующих, ранжируя проекты на основании даты старта и количества трудозатрат. Существует <a href="/images/milestones/9.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">табличка</a> в Office 365, где мы ведем список дел, строим планы и т.д. В ней отмечено количество трудозатрат по данным проектной карты. Но время-то идет, проекты растут и развиваются, каждый раз сверяться с данными из JIRA – устанет рука.
Мы создали Power Automate flow, который может реагировать на получение HTTP запроса извне. Сам endpoint создается автоматически, вам остается задать json schema входящих данных и настроить их использование. На <a href="/images/milestones/15.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">скриншоте</a> видно, что на входе – список названий проектов с их трудозатратами, которые используются для <a href="/images/milestones/7.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">обновления</a> строк таблицы. Endpoint есть, дело за малым – настроить еще один flow, запускающийся по <a href="/images/milestones/17.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">расписанию</a> (можно стартовать и вручную), который <a href="/images/milestones/1.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">собирает</a> все названия проектов из Excel таблицы и <a href="/images/milestones/16.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">дергает</a> простенький микросервис на kotlin.

```kotlin
projectCards(env[TOTAL_EFFORTS_FIELD]).associateBy { it.summary }.let { mapping ->
  HttpClient(ClientCIO) { install(JsonFeature) }
    .post<Any>(env[CALLBACK_URL]) {
    body = call.receive<Array<Summary>>()
      .sortedBy { it.summary }.mapNotNull { project ->
        mapping[project.summary]?.getField(env[TOTAL_EFFORTS_FIELD])?.value?.let { efforts ->
          Effort(project.summary, (efforts as Double / 168).roundTo(2))
        }
      }
  }
}
```

Таким образом, каждый день в Excel файле будут свежие данные о "размере" проекта.

## Работа с качеством данных в проектных картах

В проектных картах сменился формат поля Customer region. Теперь оно с автодополнением, стандартизированного формата. Но всего не предусмотришь, кто-то может и адрес в него вписать. Как же блюсти качество данных, реагировать чтоли на почтовые нотификации об изменении значения этого поля от джиры?.. Решением стал <a href="/images/milestones/2.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">скрипт</a> автоматизации в Excel 365, который "ходит" за данными, добавляет их в <a href="/images/milestones/8.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">таблицу</a>, строит <a href="/images/milestones/14.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">Pivot table</a> и даже добавляет <a href="/images/milestones/10.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">Pie chart</a>. Даже тут Microsoft делает все возможное для того, чтобы такой автоматизацией могли пользоваться не только программисты – крутой <a href="/images/milestones/5.jpg" class="fresco" data-fresco-options="ui: 'inside', thumbnails: false">intelli-sense</a>, возможность записи макросов и просмотр получившегося кода, консоль отладки. За данными скрипт ходит в небольшой микросервис на kotlin.

```kotlin
projectCards(env[CUSTOMER_REGION_FIELD]).let { cards ->
  val regions = cards.mapNotNull { it.getField(env[CUSTOMER_REGION_FIELD])?.value?.toString() }
  val map = regions.groupingBy { it }.eachCount()
  val size = regions.maxOf { it.split(", ").size }
  call.respond(Statistics(size, regions.distinct().map { region ->
      Region(region.split(", ").plus(generateSequence { "" }.take(10)).take(size), map.getValue(region))
  }))
}
```

Да, решение "наколеночное", масшабировать его будет затруднительно. Однако прелесть в том, что для быстрой автоматизации для "проверки идеи" Power BI не нужен, достаточно и такого "наколенника".

## Выводы

Смело могу советовать Power Automate как средство для автоматизации мелких процессов как внутри проекта, так и на уровне компании. Vendor-lock максимальный, но мы же не программную систему на нем строим, а только решаем мелкие прикладные задачи.
Возможности всей [Power Platform](https://powerplatform.microsoft.com/en-us/) сильно шире скриншотов из этой заметки. Вот классная [статья](https://habr.com/ru/post/520926/) на эту тему.

