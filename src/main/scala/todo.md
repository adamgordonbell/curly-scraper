
* do prism and Optional exerices
* typeclass syntax operators 
* define monoid instance for Map where keys are monoids 
* natrual transformation
* jdegoes -> pick something in cats / scalaz that looks sufficently scary and then try to understand it
  * start by trying to undersnad the type constructor, draw a pick for yourself, look at the laws
* try running interpreter
* build 
* unfold web parser

* tail recursive fix
to turn a recursive function to tail recursive, pass all your stack as parameters

Ideas:
* reattend future one remotely?
* contact john for something to contribute to the zio project


-----
Daniel Liu @DanielL49701036_twitter 09:50
Can you give the key words with which one can search for the Haskell course?

Dmytro Kostiuchenko @edio 09:50
@DustinMichaels FWIW, I asked similar question, and was suggested this course on category theory
https://www.youtube.com/watch?v=I8LbkfSSR58&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_

Just started it, so can't comment how well does it cover all those things (if at all)


Dejan Mijić @mijicd 09:50
@DanielL49701036_twitter NICTA is quite good one

DustinMichaels @DustinMichaels 09:50
Thanks Dmytro I'll check it out

Daniel Liu @DanielL49701036_twitter 09:50
@DustinMichaels Thank you!

Vinh Vu @Vinh-Vu-ck 09:50
I highly, highly recommend Haskell Programming First Principles. Super long (1200 pages), but it explains everything extremely well and thoroughly

Adam Bell @agbell 09:51
what is NICTA?

Dejan Mijić @mijicd 09:51
@agbell https://github.com/data61/fp-course

Adam Bell @agbell 09:52
Typeclassopedia is a highlevel diagram / overview of these abstractions in haskell
@mijicd thanks

realfiretiger @realfiretiger 09:52
kotlin + arrow libs is pretty good if you want fp but scala & haskell are not an option

Salar Rahmanian @softinio 09:53
I am intrigued by this book that I came across lately anyone read it? https://alvinalexander.com/scala/functional-programming-simplified-book

DustinMichaels @DustinMichaels 09:53
I've read it @softinio. It is pretty good for people new to functional programming. Good to share with junior engineers on the team trying to use functional programming for the first time.

Daniel Liu @DanielL49701036_twitter 09:53
Is Scala 3 gonna sway from Scala 2 so significantly that we need to relearn a lot of the stuff?

Oli @Oli_kitty_twitter 09:54
https://www.scala-exercises.org/fp_in_scala/getting_started_with_functional_programming Exercises from the Red book

https://typelevel.org/cats/nomenclature.html


* ref uses compare and set
  * wouldn't it be more performant to have it on it's own fiber and have a mailbox
  * sort of an actor approach
* use zio and queus to aggregate data from several kafka sources into one kafka message
  
  
  put - str - line
  
  
  