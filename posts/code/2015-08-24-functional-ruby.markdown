---
title: How To Leverage Ruby's Functional Programming Capability
tags: ruby
language: english
---

Ruby is a multi-paradigm programming language. It fully allows writing old-fashioned procedural code, but also provides many useful constructs and features from the functional world.

The majority of developers come to [Ruby](https://web.archive.org/web/20220521011426/https://www.programmableweb.com/news/why-parse-picked-googles-go-over-microsofts-c-to-replace-its-ailing-ruby-stack/brief/2015/06/16) from the imperative world. They are used to making many local variables, changing their state and relying on implicit dependencies. Very quickly, it becomes clear that code can be much more expressive, using powerful idioms from functional languages. Ruby isn't a fully functional language by any means; functions are not first class citizens, evaluation flow is not lazy, pattern matching support is very limited, etc. But still, it is possible to write code in a functional way and garner many benefits as a result.

<!--more-->

I'd like to start with a practical example. Let’s define a problem, try to solve it using both imperative and functional styles in Ruby and see what happens. Let me first point out that it’s very hard to come up with a good example that is concise and easily understandable and, at the same time, not too artificial, but I have done my best.

**Problem definition:** Write a Function, which accepts a list of users with a `full_name` property and returns a string with users' names and birthdays sorted by distance from the current point in time. Birthdays of users can be obtained from an external system by using `birthday` method of `BirthdayRegistry` class. If multiple people are lucky enough to be born on the same day, the function should combine them together with a comma.

**Example:** Bob and Joe are both born on July 16, 1985, Maria celebrates her birthday on January 2, 1989, and Alice blows out her candles on October 25, 1989. The function should return `"[Alice] - 1989-10-25; [Maria] - 1989-01-02; [Bob, Joe] - 1985-07-16"` string.

**Imperative implementation:**

```ruby
def birthday_sequence(users)
  result = ''
  hash = {}
  users.each do |user|
    birthday = BirthdayRegistry.birthday(:date, user)
    hash[birthday] ||= []
    hash[birthday] << user
  end

  sorted = hash.sort_by { |birthday, _| (Date.today - birthday).abs }

  sorted.each do |birthday, celebrators|
    result << '['
    names = []
    celebrators.each { |user| names << user.full_name }
    names.sort!
    names[0..-2].each do |name|
      result << name
      result << ', '
    end
    result << names.last + "] - #{birthday}; "
  end
  result[0..-3]
end
```

As I mentioned earlier, this example is a bit artificial, but code similar to this can be easily found in an arbitrary Ruby project featuring several junior developers. My hope is that you, as the reader, will practice some patience with this example since it is only used for demonstration purposes of the article.

Let’s rewrite this Function using functional style, applying well-known Ruby idioms.

```ruby
def birthday_sequence(users, registry = BirthdayRegistry, today = Date.today)
  users.group_by(&registry.method(:birthday).to_proc.curry[:date])
    .sort_by { |birthday, _| (today - birthday).abs }
    .map { |birthday, celebrators| "[#{celebrators.map(&:full_name).sort.join(', ')}] - #{birthday}" }
    .join('; ')
end
```

This looks much more concise than the original variant. Even after refactoring of the former (keeping imperative style intact), it will remain longer. This is a very common side effect of writing programs in functional style. It _forces_ you to express _what_ is in the code, instead of _how_. Let’s review the most interesting parts of the second code example.

Please pay extra attention to the absence of variables in the method written in functional style. How much easier is it to extract formatting code out of it? You don't have to scan the method, detecting all places where the result variable is used.

Data transformation and filtering (`map`, `collect`, `inject`, `reduce`, `filter`, `detect`, `reject`, `zip`, etc.) – that is what makes Ruby, as well as other functional languages, so expressive and concise. All developers new to Ruby learn the usefulness of these functions first. Indeed, it’s much more practical to just describe _what_ to do with data, instead of writing nasty _for_ loops. `users.map(&:full_name)` will iterate through users, extracting value of `full_name` property from each of them and returning an array of `full_names`. The `join` function will combine everything together, separating values by a comma followed by a space.

`group_by` is a function, which groups an input array into _buckets_(arrays) by result of block evaluation on each value. Given an array of strings: `['foo', 'bar', 'buzz']`, `group_by { |string| string.length }` will return `{ 3 => ['foo', 'bar'], 4 => ['buzz'] }` hash. I know, it doesn’t look like a completely fair substitution (in the original piece of code it’s done 'by hand'), but `group_by` as well as `index_by` and similar concepts are very well known and accepted in functional languages. Developers use such data transformations as building blocks, combining them with each other to achieve the desired result instead of describing what the computer should do during each step.

`.method`. In Ruby, it’s a way to get a method object – 'pointer' to a method. Here we are getting a pointer to `birthday` method of the `registry`. The `&` symbol converts method to a block, which can be then passed to any method expecting one. For example: `5.method(:modulo).call(2)` will give the same result as `5.modulo(2)`. This is a common way to pass a method instead of a block. But just getting a method isn't enough, `BirthdayRegistry.birthday` also accepts format as a first argument.

The trick is to _curry_ that pointer to a method. In functional languages, currying means partially applying arguments to a function. A curry operation takes a `proc` of N arguments and returns a `proc` of one argument, which returns a `proc` of one argument, which returns... N times – you get the idea. In the functional code example, we are currying the `birthday` method, providing the first argument to it (`call(:date)` notation is substituted with `[:date]` notation for shortness – Ruby has many ways to call a function). Having done that, the result can be used in the `group_by` function as a block.

The sorting part looks essentially the same in both examples with one minor difference – but a very important difference. Imperative code just uses `Date.today` to get the current date. This is a reference to a global, non-pure state! The result of `Date.today` is different each time (day) we call it. Having `Date.today` engraved into the function body makes it very hard to test without the _magical_ [timecop](@gh(travisjeffery):timecop) gem (which monkey patches `Date` and can stop time for a while). Not to mention the incorrect behavior of the `birthday_sequence` function itself – for each user today can be different and, therefore, the time difference between birthday and today is different. Just imagine yourself debugging a defect, from the QA team about 'off by hour' shift in the middle of the user's birthday string only twice a year.

The solution to that problem is also dependency injection. This is not a functional paradigm concept at all, but almost every functional program uses it. For a function to be pure, it’s not allowed to operate in an external global state (otherwise, it will return non-deterministic results). So, instead of referring to a global state, we inject a variable inside a function through its parameters. Doing so, we eliminate the possibility of an 'off by hour' defect to even appear (each time the difference is now calculated with the same 'now' value).

Purity is, probably, the most loved concept in functional languages. A function that does not depend on any external state always returns the same result, is very testable, reusable and easy to understand. In the majority of cases, it is also much easier to debug such a function. Actually, no debugging is needed; you just call a function with some arguments and inspect the result. There is no way for the external world (the rest of the system) to influence what pure function is going to return. The signature `def birthday_sequence(users, registry = BirthdayRegistry, today = Date.today)` injects dependencies of a function from the outside instead of referencing them from the function body. Just looking at a function signature makes it clear for other developers that it actually uses today inside, falling back to `Date.today` by default, if nothing was passed. With such a signature, we can make a function pure, as soon as `BirthdayRegistry.birthday` is also pure.

The injection of `BirthdayRegistry` doesn’t look like a big deal, but it’s hard to underestimate it. This little injection has a huge implication on testing. If you are a good developer, you write a couple of unit tests to ensure that the `birthday_sequence` function works as expected. Before calling it and asserting the result, however, you need to set up an environment. You need to make sure that `BirthdayRegistry.birthday` will actually return data for users on which you are testing your function. Therefore, you have a choice of seeding an external storage (from which `BirthdayRegistry` takes its data) or Mocking the implementation of the birthday method. The latter is easier, so you do `allow (BirthdayRegistry).to receive(:birthday).with(anything, user).and_return(...)`. Now, look at your unit test. Developers who will read it later will have no clue why you are setting up a `BirthdayRegistry` mock before calling the `birthday_sequence` function without looking at its implementation. Congratulations, you now have a semantic dependency! Every time you decide to work with the `birthday_sequence` function, you'll have to keep in mind that it’s actually calling `BirthdayRegistry` inside. The injection allows you to pass stub implementation of `BirthdayRegistry` in the unit test explicitly, without semantic dependency (if the method accepts it in parameters, I would bet it’s using it).

Comparing code from [imperative_fake_spec.rb](@gh(maksar):functional_ruby_article/blob/master/imperative_fake_spec.rb) and [imperative_real_spec.rb](@gh(maksar):functional_ruby_article/blob/master/imperative_real_spec.rb) tests, it’s not easy to see the difference, but it’s crucial for test feedback loop speed. Just stubbing out `BirthdayRegistry` dependency, we gain speed – lots of speed. Since unit tests are not hitting databases or any other external storage, they can work lightning-fast. The functional code test [functional_spec.rb](@gh(maksar):functional_ruby_article/blob/master/functional_spec.rb) encourages passing fake implementation of external dependency, leaving no chance to test slowness.

<a href="https://asciinema.org/a/21318" target="_blank"><img src="https://asciinema.org/a/21318.svg" class="center full"/></a>

Full sources of examples and unit tests can be found at [GitHub](@gh(maksar):functional_ruby_article) repo.

There are many other areas in which functional languages can affect the way you write Ruby code: [Monads](@w:Monad_(functional_programming)), [higher order functions](@w:Higher-order_function), [immutability](@w:Immutable_object), etc. My goal in this article was to demonstrate the basic elements of functional programming in Ruby and inspire developers to discuss this issue further in the hopes of learning to code better and faster.