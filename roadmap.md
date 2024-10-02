# Roadmap

If you like what you see now, that's pretty much what Elm is going to be for a while.

I'm currently doing some exploratory work with compiler techniques and targets, but it is still too early to tell which parts of that might work out. I know many readers take "exploring X" as a promise that "X will definitely happen", so I want to be careful in setting expectations low since there is still so much uncertainty in the work. So while there is no real news to share at this point, I can give some expectations about future stability of Elm:

**If this exploratory work goes well**, even in the wildest version of success, I wouldn't expect the language or core packages to change very much. Maybe two or three years down the line it could reveal something that'd be good to fix up, but I don't really foresee notable changes right now.

**If this exploratory work does not work out**, I have some more conservative projects that are good ideas that I want to circle back to at some point. Things like:

- add a constrained type variable `eq` to get rid of the runtime error for `(==)` on functions
- try to integrate `elm test` so it can run only tests that changed
- try to get `elm format` using the latest parsing infrastructure so it's not a perf bottleneck during development anymore
- do another round on perf because I have one last idea that can squeeze out a bit more speed

These are all nice quality of life things, but with 0.19.0 and 0.19.1 taking so long, I got pretty burnt out on "incremental improvements that are good ideas, but take a very long time and aren't very exciting to non-Elm users." Without getting too into the details, I really needed to change things up before returning to these particular ideas.

If someone has a security issue from the compiler or core libraries, please DM me about it on the Elm Slack. Outside of security issues, I think capacity for more discretionary changes will increase once the lessons from the compiler explorations become more clear. Hopefully the discussion [here](https://discourse.elm-lang.org/t/costs-funding-in-open-source-languages/5722) clarifies the thinking on these capacity questions a bit.

<br>
<br>

**Taking a step back**, I have found that working in this looser style has produced a high baseline of quality, and I think that is an important part of Elm. For example, all the error message work in Elm began as a project to implement the `--report=json` flag. That work happened to reveal some cool ideas on improving error messages, and if I had been using a rigid roadmap, I might have skipped those ideas to meet the publicly-stated arbitrary deadline. We'd have a clearer roadmap, but error messages no different than other type-inferred languages.

So I think having more flexibility in planning is a competitive advantage for Elm in certain ways, but it is obviously a trade off that does not work for everyone. If someone needs more certainty, I generally recommend looking into other languages to see if they have a balance that works better for their needs. For example, languages made by big corporations are generally "less risky" on things like this, but I think you see the fruits of that kind of process in the design decisions as well. Trade offs!

Anyway, like I said at the beginning, if you like what you see now, that's what it's going to be for a while. Even in the best case scenario with my current explorations!

I hope this is helpful information, and I hope you have a good experience with Elm, even if you ultimately find a different language that works better for you!
