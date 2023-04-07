# servant-combinators

  This library provides combinators for endpoint handlers that aren't
provided by the existing `servant` and `servant-server`
libraries. In short, this gives access to the rest of the fields in
the raw `Network.Wai.Request` record type, including the Request
itself.

  Boundless thanks goes to William Yao's excellent [Writing Servant
Combinators for Fun and
Profit](https://williamyaoh.com/posts/2023-02-28-writing-servant-combinators.html)
blog post, which formed the foundation this work was built upon. This
library would not have been possible without his work and example.

  Now, it's possible that this library strikes you as heretical, or
working directly against the goals that Servant sets out to achieve,
namely type safety and making what a handler uses explicit and
verbose. Much effort seems to have been put into making sure that
handler authors can't just grab everything from the raw WAI
request. However, I think there are a few reasons why this is
sometimes essential, and often helpful.

  First off, it can be essential if you're passing fields
(QueryParams, Header values, etc) directly to a subroutine in a
dynamic way. I've seen this come up in production code a lot if
you're calling an internal endpoint that handles text rendering, for
example.

  Second off, it can also result in a long string of arguments of
identical types in a type signature, which in my experience is an
excellent place for bugs to hide and multiply. Indeed, the [Settings
pattern](https://www.yesodweb.com/book/settings-types) solves this
problem (among others).

  I'm an ardent believer in the value of type safety, but type safety
should not stop people from being able to do legitimate
tasks. Furthermore, collapsing a long string of easy-to-transpose
arguments into a record type with clearly named fields makes code
_easier_ to read and maintain, not harder.
