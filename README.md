# DanielBrice.net

The personal website of Daniel Brice, PhD.

Copyright © 2014-2020 Daniel Brice [CC BY 4.0][1]

## Setup Development Environment

Instructions for setting up development environment.

`gem` is the Ruby package manager.
An individual package is aptly called a "gem."
`bundle` is the Ruby sandbox manager.
The Ruby world is active, packages are improved, APIs change, and projects break.
`bundle` ensures you get the correct version of `ruby` and of each required `gem` on a per-project basis, helping you keep a consistent environment.

1.  **Make sure that `ruby`, `gem`, and `bundle` are installed**

    Ubuntu should come preloaded with `ruby` and `gem`. If not, just install them via `apt-get`.
    Once they're installed, you can install `bundle` through `gem`, but I prefer to install it through `apt-get`.

    macOS comes preloaded with `ruby` and `gem`.
    Note that as of August 2016, there is no Homebrew formula for `bundle`: You must install `bundle` through `gem`, unfortunately.
    If it asks for elevated privilages, use `gem install bundler --user-install` and add `~/.gem/ruby/<VERSION>/bin` to your path.

    I should mention that you need Ruby version 2 or higher.
    To learn more, see the [Bundler project page][2] or the [GitHub Pages help page][3].

2.  **Pull in the required Ruby gems**

    From the project directory, install the project dependencies.

    ```
    bundle install --path .gems
    ```

    The file `Gemfile` is a human-readable list of the packages required for your project.
    The file `Gemfile.lock` is a machine-readable file used by `bundle`.
    Don't mess with it, but track it as part of the project with `git`.

    **Note:** If you run into an issue running `bundle install`on macOS, you might try `brew unlink xz && bundle install && brew link xz`, as per [this][4] Stack Overflow question.

Now you're all set up.

## Update Development Environment

Instructions for keeping the development environment up to date.

1.  **Keep `bundle` up to date**

    Update `bundle` using whatever package manager you installed it with.
    I guess that would be `apt-get` on Ubuntu and `gem` on macOS.

2.  **Keep `jekyll` and external libraries up to date**

    In the project directory, invoke

    ```
    bundle update
    ```

    **Note:** If you run into an issue running `bundle update`, you might try `brew unlink xz && bundle update && brew link xz`, as per [this][4] Stack Overflow question.

That's all there is to that.

## Workflow

Instructions for updating and demoing the project.

1.  **Build site locally**

    ```
    bundle exec jekyll build
    ```

    The build artifacts are found in `./_site`

*OR*

1.  **Run local development server**

    ```
    bundle exec jekyll serve
    ```

    or with drafts

    ```
    bundle exec jekyll serve --drafts
    ```

    Server runs on `http://localhost:4000`

2.  **Publish Site**

    Simply `git push` the source code.
    GitHub will build the site from source on their build servers and deploy the build artifacts to their GitHub Pages hosts.

  [1]: http://creativecommons.org/licenses/by/4.0/
  [2]: http://bundler.io/
  [3]: http://help.github.com/articles/using-jekyll-as-a-static-site-generator-with-github-pages/
  [4]: http://stackoverflow.com/questions/39937394/
