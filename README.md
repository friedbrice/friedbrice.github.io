# DanielBrice.net

The personal website of Daniel Brice.

Copyright 2014 Daniel Brice [CC BY 4.0][1]

## Development Environment Setup

Instructions for setting up development environment.

`gem` is the Ruby package manager. An individual package is aptly called
a "gem." `bundle` is the Ruby sandbox manager. The Ruby world is active,
packages are improved, APIs change, and projects break. `bundle` ensures
you get the correct version of `ruby` and of each required `gem` on a
per-project basis, helping you keep a consistent environment.

1.  Make sure that `ruby`, `gem`, and `bundle` are installed.

    Ubuntu 15.10 should come preloaded with `ruby` and `gem`. If not, just
    install them. You can install `bundle` through `gem`, but I prefer to
    install it through `apt-get`. To learn more, see the
    [Bundler project page][2] or the [GitHub Pages help page][3].

    I should mention that you need Ruby version 2 or higher.

2.  Pull in the required Ruby gems.

    ```
    bundle install
    ```

    The file `Gemfile` is a human-readable list of the packages required for
    your project. The file `Gemfile.lock` is a machine-readable file used by
    `bundle`. Don't mess with it, but track it as part of the project with
    `git`.

Now you're all set up.

## Workflow

Instructions for keeping the dev environ up to date and for updating the
project.

1.  Keep `bundle`, `jekyll`, and `ruby` up to date.

    ```
    bundle update
    ```

2.  Build site.

    ```
    bundle exec jekyll build
    ```

3.  Run local development server.

    ```
    bundle exec jekyll serve
    ```

    or with drafts

    ```
    bundle exec jekyll serve --drafts
    ```

  [1]: https://creativecommons.org/licenses/by/4.0/
  [2]: http://http://bundler.io/
  [3]: http://help.github.com/articles/using-jekyll-as-a-static-site-generator-with-github-pages/
