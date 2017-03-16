6:30:22 PM tom7942: anyone have hints on this? 

write a filter function for Foldable types using foldMap:

filterF :: (Applicative f, Foldable t, Monoid (f a))

        => (a -> Bool) -> t a -> f a  


6:30:47 PM tom7942: not sure what is meant by Monoid (f a) constraint
6:31:26 PM nitrix: tom7942: Have you seen Foldable yet?
6:31:32 PM tom7942: yeah
6:31:57 PM nitrix: The Monoid constraint arises from the use of foldMap (which is also what this exercise is asking to use).
6:32:28 PM nitrix: > getSum $ foldMap Sum [1,2,3]
6:32:30 PM lambdabot:  6
6:34:33 PM nitrix: tom7942: Have you seen monoids yet?
6:34:38 PM tom7942: yep
6:35:31 PM nitrix: Let's see if I can implement it and then give tips ￼
6:37:27 PM tom7942: i'm thinking something like this, but it's not quite right: 

filterF f = foldr (\x acc -> if f x then Just x else Nothing) mempty
6:37:40 PM tom7942: (and it obviously doesn't use foldMap yet)
6:38:06 PM tom7942: errr no,
6:39:51 PM tom7942: more like this: 
6:39:59 PM tom7942: filterF' f = foldr (\x acc -> if f x then pure x `mappend` acc else acc) mempty
6:45:51 PM nitrix: I'm trying to understand how the foldMap fits in this ￼
6:46:10 PM nitrix: What's that exercise from?
6:47:52 PM tom7942: bitemyapp book chapter 20 foldable exercises 
6:48:19 PM tom7942: yeah, i was figuring that if I could write it with foldr, i'd understand it enough to refactor to foldMap
6:48:21 PM Axman6: :t foldMap
6:48:23 PM lambdabot: (Monoid m, Foldable t) => (a -> m) -> t a -> m
6:49:16 PM Axman6: tom7942: can you write a function of type: (Applicative f, Monoid (f a)) => (a -> Bool) -> a -> f a?
6:50:15 PM Axman6: remembering that Applicative gives you pure :: a -> f a, and monoid gives you mempty :: a (or f a in this case)
6:52:43 PM tom7942: yes ￼
6:53:29 PM Axman6: what's it look like?
6:53:57 PM tom7942: f pred x = if pred x then pure x else mempty
6:54:12 PM Axman6: looks pretty good to me
6:55:04 PM Axman6: so we need to use foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m to produce (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
6:55:18 PM Axman6: what happens if we set m = f a?
6:55:41 PM nitrix: foldMap :: (Applicative f, Monoid (b f), Foldable t) => (a -> f b) -> t a -> f b
6:57:02 PM nitrix: I did  m ~ f b.
6:57:04 PM Axman6: tom7942: time to glue everything togeher! filterF pred x = ...
7:00:02 PM nitrix: I think I solved it.
7:00:44 PM nitrix: I was wondering how you'd supposedly "remove" elements and yet mappend everything with foldMap, until I clicked with mempty and pure ￼
7:02:51 PM Axman6: when you see something with a very general type like that and some class constraints, you should immediately be thinking "what does these classes give me?"
7:04:30 PM nitrix: tom7942: filterF f xs = foldMap the list xs, with the fold being that if `f` applied to the current element is True, we keep it, otherwise, we don't.
7:06:06 PM nitrix: 21:39:41         tom7942 | filterF f = foldr (\x acc -> if f x then Just x else Nothing) mempty
7:06:08 PM nitrix: 21:42:12         tom7942 | filterF' f = foldr (\x acc -> if f x then pure x `mappend` acc else acc) mempt
7:06:36 PM nitrix: You actually came up with two of the necessary bits instinctively, you just need to not think in terms of foldr; but foldMap instead ￼
7:09:44 PM tom7942: i think i got it
7:10:05 PM tom7942: but I have to put a type annotation on it to make it work
7:10:19 PM tom7942: > filterF' (/= '.') "foobar." :: String

"foobar"


7:10:22 PM lambdabot:  error:
7:10:22 PM lambdabot:      • Variable not in scope:
7:10:22 PM lambdabot:          filterF' :: (Char -> Bool) -> [Char] -> String
7:10:36 PM qu1j0t3: /b 15
7:10:38 PM tom7942: otherwise it blows up… not sure what I'm missing here
7:11:46 PM nitrix: My solution at http://lpaste.net/293974
7:14:28 PM tom7942: ok, that's what i wrote too
7:17:38 PM tom7942: why do you need to apply getSum twice?
7:20:05 PM nitrix: Apparently, the result of my filterF has type  Sum (Sum a)
7:20:13 PM nitrix: Sum (Sum Integer) sorry.
7:20:23 PM nitrix: Not sure why ￼
7:20:39 PM tom7942: glad I'm not alone ￼
7:22:49 PM nitrix: I edited the paste further, trying to figure it out.
7:23:34 PM nitrix: Ohhh
7:24:00 PM nitrix: tom7942: Fixed! http://lpaste.net/293974
7:24:33 PM nitrix: Re-edited again.
7:24:43 PM nitrix: tom7942: Want to know why ￼ ?
7:25:40 PM tom7942: yeah I want someone to walk me through it ￼
7:26:01 PM nitrix: tom7942: So, the implementation is fine.
7:26:37 PM nitrix: What I hadn't realised is that my list was actually a list of monoids [Sum 3, Sum 5, Sum 5]
7:27:40 PM tom7942: the list is a monoid of monoids, right?
7:27:42 PM nitrix: But if that's the case, that means `t a` gets infered as `[Sum Int]` and then the result must be `f (Sum Int)`
7:28:04 PM nitrix: In other words, I misunderstood what the function does.
7:28:28 PM nitrix: It's so powerful. The caller determines how the type is inferred and so, how the monoids are folded.
7:28:55 PM nitrix: getSum (filterF (==5) [3, 5, 5])   vs.    getProduct (filterF (==5) [3, 5, 5])
7:30:11 PM nitrix: Where the role of filterF is exactly like `filter`, it runs a predicate on a list, but it then folds them all into a single value. How the type of that monoid is inferred is going to affect the folding.
7:30:14 PM nitrix: It's very cool o:
7:30:52 PM tom7942: so is it possible to use filterF as the regular plain filter that works on lists?
7:31:54 PM tom7942: without the type annotation, that is
7:32:00 PM nitrix: tom7942: It works on any foldable by running a predicate on them, then collapse all the results that passed the predicate because they're monoids.
7:32:15 PM nitrix: ("collapse" being mappend).
7:32:39 PM nitrix: For fun...
7:32:46 PM nitrix: @let filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs
7:32:48 PM lambdabot:  Defined.
7:33:20 PM nitrix: @let demo = M.empty & M.insert "foo" 3 & M.insert "bar" 5 & M.insert "baz" 5
7:33:22 PM lambdabot:  Defined.
7:33:37 PM nitrix: > getSum $ filterF (==5) demo
7:33:40 PM lambdabot:  10
7:33:45 PM nitrix: > getProduct $ filterF (==5) demo
7:33:48 PM lambdabot:  25
7:34:28 PM nitrix: tom7942: Not just lists c:
7:39:10 PM benzrf: :t filterF
7:39:11 PM lambdabot: (Monoid (f a), Foldable t, Applicative f) => (a -> Bool) -> t a -> f a
7:39:47 PM tom7942: so I'm trying to write filter :: (a -> Bool) -> [a] -> [a] in terms of filterF 
7:40:53 PM tom7942: it just blows up with an undefined exception 
7:42:21 PM tom7942: oops, i see it now.
7:42:39 PM tom7942: all you need to do is specialize the type. that's quite nifty.
7:42:48 PM nitrix: Yeah.
7:43:03 PM nitrix: @let myMap = fmap :: (a -> b) -> [a] -> [b]
7:43:05 PM lambdabot:  Defined.
7:43:18 PM tom7942: @let filter' = filterF :: (a -> Bool) -> [a] -> [a]
7:43:21 PM lambdabot:  Defined.
7:43:21 PM nitrix: It's almost trivial to specialize more generic functions ￼
7:43:37 PM tom7942: this will take some time to solidify in my mind ￼
7:44:08 PM nitrix: I've been doing Haskell for a while and I had to think a little there. I'm still very productive so I wouldn't worry too much.
7:44:18 PM tom7942: heh, good to know
7:44:27 PM nitrix: It's a lot of fun though; clashing Foldable, Applicative and Monoid all at once.
7:45:06 PM nitrix: tom7942: How's the book so far ￼ ?
7:45:15 PM tom7942: i'l still a bit uncertain about the Monoid (f a) constraint. i don't think i've seen that until now.
7:45:28 PM tom7942: nitrix: fairly rigorous 
7:46:25 PM tom7942: nitrix: i find the practical haskell examples shown to be too web focused for me to really decipher
7:47:22 PM nitrix: The idea is that `Sum a` is both a Monoid and an Applicative.
7:47:59 PM nitrix: Well, `Sum` is the applicative.
7:48:08 PM tom7942: yes, i was going to say…
7:48:22 PM nitrix: I'm very tired x]
7:48:27 PM tom7942: so the Sum a must be a monoid
7:48:34 PM tom7942: does that mean that a must be a monoid too?
7:48:35 PM nitrix: Yup.
7:48:44 PM nitrix: tom7942: Nope.
7:48:51 PM nitrix: That's the point precisely.
7:50:08 PM nitrix: `Monoid (f a)`   so  `Num a => (Sum a)` is a monoid, `Num a => a` isn't a monoid.
7:50:12 PM tom7942: so it really is saying that the Applicative f, must be applied to be a Monoid?
7:50:49 PM nitrix: The instance's here:  instance Num a => Monoid (Sum a)
7:51:34 PM nitrix: tom7942: Well, let's specialize it.
7:51:39 PM nitrix: :t filterF
7:51:41 PM lambdabot: (Monoid (f a), Foldable t, Applicative f) => (a -> Bool) -> t a -> f a
7:52:14 PM nitrix: (Monoid (Sum a), Foldable t, Applicative Sum) => (a -> Bool) -> t a -> Sum a
7:52:19 PM nitrix: See how it's the same `f` ?
7:52:40 PM nitrix: (Monoid (Sum Int), Foldable t, Applicative Sum) => (Int -> Bool) -> t Int -> Sum Int
7:52:48 PM nitrix: (Monoid (Sum Int), Foldable [], Applicative Sum) => (Int -> Bool) -> [Int] -> Sum Int
7:52:58 PM nitrix: At which point we can remove the constraints.
7:53:06 PM nitrix: (Int -> Bool) -> [Int] -> Sum Int
7:53:20 PM tom7942: excellent, thank you!
7:53:40 PM nitrix: Thus why I said earlier, it's clever because it's the caller that determines the type of `f`.
7:53:54 PM nitrix: So, the monoid used during folding.
7:55:32 PM tom7942: mind warping stuff ￼
7:55:54 PM nitrix: > getFirst $ fold First [Nothing, Just 5, Just 7]
7:55:57 PM lambdabot:  error:
7:55:57 PM lambdabot:      • Couldn't match type ‘First a0’ with ‘[Maybe Integer] -> First a’
7:55:57 PM lambdabot:        Expected type: Maybe a0 -> [Maybe Integer] -> First a
7:56:02 PM nitrix: > getFirst $ foldMap First [Nothing, Just 5, Just 7]
7:56:05 PM lambdabot:  Just 5

