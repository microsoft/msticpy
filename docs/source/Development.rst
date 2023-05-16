MSTICPy Development Guidelines
==============================

Contributions of improvements, fixes and new features are all
welcomed. Whether this is your first time contributing to a
project or you are a seasoned Open-Source contributor,
we welcome your contribution. In this guide you can find a few
pointers to help you create a great contribution.

What to contribute
------------------

There are many things that can make a good contribution.
It might be a fix for a specific issue you have come across,
an improvement to an existing feature that you have thought
about such as a new data connector or threat intelligence provider,
or a completely new feature category.

If you don’t have a specific idea in mind take a look at the
Issues page on GitHub: `https://github.com/microsoft/msticpy/issues`__

This page tracks a range of issues, enhancements, and features that
members of the community have thought of. The MSTICPy team uses these
issues as a way to track work and includes many things we have added ourselves.

The issues are tagged with various descriptions that relate to the
type of issue. You may see some with the ‘good first issue’ tag.
These are issues that we think would make a good issue for someone
contributing to MSTICPy for the first time, however anyone is welcome
to work on any Issue. If you decide to start working on an Issue please
make a comment on the Issue so that we can assign it to you and other
members of the community know that it is being worked on and don’t
duplicate work. Also if you are unclear about what the Issue feel
free to comment on the Issue to get clarification from others.



What makes a good contribution?
-------------------------------

Whilst there is no one thing that makes a contribution good here are some guidelines:

Scope
~~~~~
Focus your contribution on a single thing per PR (Pull Request) raised, whether it
be a feature or a fix. If you have multiple things you want to contribute,
consider splitting them into multiple PRs. Keeping each PR to a single item
makes it easier for others to see what you are contributing and how it
fits with the rest of the project.

Documentation
-------------
Make it clear what you are contributing, why its important, and how
it works. This provides much needed clarity for others when reviewing
contributions and helps to highlight the great value in your contribution.

Unit test and test Coverage
---------------------------
Write unit tests for your code. We use `pytest <https://pytest.org>`__
to run our tests.

See the section :ref:`dev/CodingGuidelines:Unit Tests` for more information.

Using Git
---------
To contribute you will need to fork the MSTICPy repo.
**Create a branch** for your contribution, make the code changes
and then raise a PR to merge the changes back into
MSTICPy's main branch. Please *do not* make changes to `main` of your
fork and submit this as a PR.
You should also consider granting permission on your fork so that
we can push changes back to your forked branch. Sometimes, it's
quicker for us to make a quick change to fix something than to ask
you to make the change. If we cannot push any changes back
this is impossible to do.

If you are unfamiliar with Git and GitHub you can find some
guidance here: https://docs.github.com/en/get-started/quickstart/set-up-git


Where to get help
-----------------
We are more than happy to help support your contributions,
if you need help you can comment on the Issue you are working on,
or email [msticpy@microsoft.com](mailto:msticpy@microsoft.com)

You can also join our Discord
`#msticpy <https://discordapp.com/channels/717911137915764877/922881584288399410>`.


.. toctree::
   :maxdepth: 2

   dev/CodingGuidelines