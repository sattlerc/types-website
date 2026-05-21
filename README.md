# Source code for TYPES 20XX conference website

This is a static website generated using [Hakyll](https://jaspervdj.be/hakyll/).
The source is hosted in the following Git repository:
```
types-20XX@example.com:website
```
Send XX your public SSH key to get access.

If you want to generate the site locally on your computer to preview your edits, you need an installation of GHC and Cabal.
But if you are happy to just make edits without previewing, you can skip this.
Then just edit the pages and push to the above repository.

## Basic Git workflow

1)  Make sure you have Git installed.
2)  Run `git clone types-20XX@example.com:website` to clone the repository.
    This will create a new folder `website` for the repository on your computer.
3)  Work with the repository using:
    - the [command line](https://www.w3schools.com/git/git_workflow.asp?remote=github),
    - or your favourite graphical Git client, for example [GitHub Desktop](https://github.com/apps/desktop):
      + skip over the GitHub account creation (we are not using GitHub here),
      + *add local repository* and select the folder you cloned before,
      + when you edit files in the folder, the changes show up here to be committed and pushed.

## Generation

To generate, run:
```
cabal run site build
```
This will generate the website in the subdirectory `_site`.

The generation process is defined by the `site` executable (see `site.hs`).
For most purposes, you should not need to edit this.
If you do, you must regenerate the website using `rebuild` instead of `build`.

## Structure and editing

The different pages of the website are defined in Markdown or HTML in the top-level directory (example: `call-for-constributions.md`).
You can edit these and add new files.

Pages are rendered using the template `templates/default.html`.

The navigation menu is defined in `templates/navigation.html`.
Edit the list in the metadata to add or remove pages in the menu.

Styling is defined in `css/main.css`, included by `templates/default.html`.

Files in `files` and `images` are copied over as is.

Abstracts are in `abstracts` (named by submission id) and copied over as is.

There is an inclusion mechanism to avoid content duplication.
Files to include are placed in the `include` subdirectory.
They are compiled as regular pages.
They can be included using the template key `\$include_<filename_stem>\$`.

Some pages embed automatically generated HTML blocks using template keys.
It is recommended to use them inside an HTML-block element to prevent Pandoc from generating an ill-typed paragraph around them.
For example:
```
<section>\$accepted_papers_list\$</section>
```

These are computed in `Papers.hs` from the following data files:
* `papers.json` (downloaded from the HoTCRP instance),
* `invited.json`,
* `sessions.json`,
* `schedule.json`.

Supported template keys:
* `papers_list`: uses `papers.json` and the directory `abstracts`,
* `invited_list`: uses `invited.json` and images in `images/invited` with corresponding basename,
* `programme_table`: uses `invited.json` and `sessions.json` and `schedule.json`,
* `programme_list`: uses all of the above data files and the directory `abstracts`,
* `organizing_committee`: uses `organizing_committee.json`,
* `program_committee`: uses `program_committee.json`,
* `steering_committee`: uses `steering_committee.json`.

## Preview

It is convenient to preview your changes while editing.
For this, keep the following command running:
```
cabal run site watch
```
and point your browser to [http://localhost:8000/](http://localhost:8000/).
The site will automatically build when you make changes.
So you just have to refresh the page in your browser.

## Deploying

The website is intended to be deployed to <https://example.com>.
To deploy, simply push your commit to the main branch of the above repository.
The push will fail if the website fails to generate or deploy.

## Sharing drafts

Feel free to create and push draft branches.
Other people can then look at your proposed changes before they are merged into the main branch.
