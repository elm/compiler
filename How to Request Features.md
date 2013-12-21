# How to make a Feature Request

Adding features is hard work and has long term implications for Elm.
If you think your particular feature is important, help make it happen
by doing some of the background work that goes into actually adding it.

### Before making a Request / Formatting:

  - Make sure your feature has not already been requested. Look through the
    email list to find similar requests.
  - Submit feature requests by emailing the list. This allows the broadest
    discussion and does not clutter up the GitHub repository with speculative
    issues. I'd prefer to use the GitHub issue tracker for bug tracking and
    issues with the existing code base.
  - Label all feature requests with a subject like "Request: _____". For
    example, if you want to be the umpteenth person to request type classes,
    you would use the subject "Request: Type Classes".

### Body of the Request:

Your request should address all of the following issues:

**General Problem:** Clearly define the purpose of adding the feature.
What general problem do you want to solve? Why is that problem important?
Why is it important for a functional GUI language?

**Specific Use-Cases:** What are the specific cases when the general problem
is actually an issue? How often does this case come up? Why are these cases
important to easily designing graphical user interfaces? Why are existing
solutions in Elm unsuitable?

**Background Materials:** Are there relevant papers on the topic? Are there
languages or libraries that already have this feature? Organize this material
and provide a concise and helpful overview.

**Alternatives:** What are all the ways to address the general problem or a
closely related problem? For example, the semantic flexibility of type classes
can be largely replicated with a module system in the style of ML.
What are the strengths and weakness of each of the alternative solutions?

**Suggested Approach:** Outline your specific approach. How does it address
the general problem? How does it work? How hard is it going to be to add?
What other language features does it depend on that need to be added first?
Try to take a look at the relevant code to see if it would require significant
changes. If you are suggesting an API or library changes, include an interface
(a list of type signatures) for the functions you would like to add (perhaps
multiple interfaces if there are many good approaches).

**Known Issues:** Search for issues that people have already found. Think of
any other possible downside and list them too. For example, one downside of
typeclasses is that they are globally defined for each type, so you cannot
have multiple `Ord` instances for Strings. This can lead to namespace pollution
within a project. In general you can only implement an "interface" once per
type, so it is very tricky to get the flexibility of ML's module functors.

**Justification:** Why is the added complexity of your feature worth it? Discuss
both implementation complexity in the compiler, and conceptual complexity that
users must deal with. Why is your feature better than the alternative solutions?
Why is it okay to ignore the known issues? Is there any way to overcome the known
issues? Will it be difficult or confusing for users coming from JavaScript?

### Some final notes:

If for some reason you cannot provide details for some of these categories,
explain why the category is not applicable.

Re-read before posting. Is your request constructive? Is it well organized?
Are there any details you forgot? Can anything be clarified or trimmed down?
Considering a request takes time, so be considerate about how much work a
reader has to do to take it seriously.

A feature request is not an opportunity to be sassy or glib. Try to present
the information and argument clearly, concisely, thoroughly, and in an unbiased
manner.

This is likely to be an iterative process. People will have comments and
questions. Everyone please be kind. Commenters, if you think you have a
concern, raise it as a question: "But what happens when you do blah?" instead
of "This will break when you do blah". Assume you have misunderstood their
proposal or only have a shallow understanding of it. Seek clarification, not victory.

Comments are not a place for flame wars. Discuss specific issues. Vague
complaining and unsupported claims should either be forgotten or developed
into specific and well-supported concerns.

I am always working hard to add cool stuff to Elm, and it is a lot of work.
I must prioritize issues to focus on the things that have the highest impact
for improving the quality of Elm, addressing the concerns of current users,
and making Elm more appealing for new users. I do not have infinite time,
so some things must receive more attention than others. I apologize in advance.

And to summarize, just be cool! I want Elm to be really fun and easy to use,
and I want your projects to be amazing!