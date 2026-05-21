# Deployment setup

1. Find a *web server* to host a static website.

2. Find a server `REMOTE_HOST` with a user account `REMOTE_USER` that you can share with everyone who should be able to edit the website.
   We will refer to this as the *remote server*.
   Add the public SSH keys of everyone who should have access to `~/.ssh/authorized_keys`.

3. On the remote server, create a bare git repository at some directory `REMOTE_REPO`:

   ```shell
   git init --bare
   ```

   This will serve as remote website repository.
   On your local computer, add it as remote to your website repository (this repository):

   ```shell
   git remote add origin REMOTE_USER@REMOTE_HOST:REMOTE_REPO
   ```

   Push your website skeleton.
   Other website editors can then clone this:

   ```shell
   git clone REMOTE_USER@REMOTE_HOST:REMOTE_REPO
   ```

4. On the remote server, write a script `DEPLOY_SCRIPT` that deploys a directory given as argument to your web server.

   Here is an example script, assuming that:
   * you have SSH access to the web server at `WEB_USER@WEB_HOST`,
   * the web server is  configured to serve directory `WEB_PATH`.

   ```bash
   #!/bin/bash
   rsync --verbose --recursive --delete --delete-delay --links --times "$1" WEB_USER@WEB_HOST:WEB_PATH
   ```

   In more complicated settings, the user hosting the remote repository might be different from the user with web server access.
   In the case the latter user has read access to the former user, the scripts in [`tools/deploy/](tools/deploy/)` provide a simple mechanism for deploying across users.
   These scripts have hard-coded constants which you will have to update according to your use case.
   As the user with web server access, install a systemd user service like `types-2026-deploy.service` (with an executable copy of `server.py`),
   Make an executable copy of `client.py` to use for `DEPLOY_SCRIPT`.

5. On the remote server, create the *tracking repository*, for example as follows:

   ```shell
   REMOTE_USER@REMOTE_HOST:REMOTE_REPO$: git clone --shared tracking_repo
   ```

   If you use a different path, substitute appropriate values in what follows.

6. On the remote server, install a reference-transaction hook in `REMOTE_REPO` as follows.
   This hook will run on pushes and take care of the following:
   * check whether this is a push to the configured branch (below, `main`),
   * update the tracking repository,
   * build the cabal project,
   * build the website using the generated executable (default: subdirectory `_site`).

   Copy `collect_output.py` and `reference-transaction-hook.py` from `tracking-repo/tools` to `REMOTE_REPO`.
   Create an executable file `REMOTE_REPO/hooks/reference-transaction` with the following content (substituting the appropriate values):

   ```bash
   #!/bin/bash
   ./collect_output.py '> ' \
     ./reference-transaction-hook.py \
       --branch main \
       --cabal-executable site \
       --docker-executable podman \
       --docker-image haskell:9.6 \
       --tracking-repo tracking-repo \
       --deploy-script DEPLOY_SCRIPT \
       "$@"
   ```

   See `REMOTE_REPO/tracking-repo/tools/reference-transaction-hook.py --help` for fine-tuning.
   You can test this hook by running it in the remote repository without arguments.
   The first time this runs, it will take a while to build the Cabal project:

   ```shell
   REMOTE_USER@REMOTE_HOST:REMOTE_REPO$: hooks/reference-transaction
   [main 6af371c] Force deploy.
   Enumerating objects: 28, done.
   Counting objects: 100% (28/28), done.
   Delta compression using up to 16 threads
   Compressing objects: 100% (21/21), done.
   Writing objects: 100% (21/21), 3.15 KiB | 1.57 MiB/s, done.
   Total 21 (delta 18), reused 0 (delta 0), pack-reused 0 (from 0)
   > Container id: dd9fe9a59db72a5d6e6f5d4f2fa076308bf69135b5237e10f1ea722bb4a1ab8e
   > Config file path source is default config file.
   > Config file not found: /cabal/config
   > Writing default configuration to /cabal/config
   > Downloading the latest package list from hackage.haskell.org
   > Package list of hackage.haskell.org has been updated.
   > The index-state is set to 2026-03-16T15:01:55Z.
   > Running Hakyll...
   > Build profile: -w ghc-9.6.7 -O1
   > In order, the following will be built (use -v for more details):
   [...]
   >  - types2026-website-0.1.0.0 (exe:site2) (first run)
   > Configuring executable 'site' for types2026-website-0.1.0.0...
   > Preprocessing executable 'site' for types2026-website-0.1.0.0...
   > Building executable 'site' for types2026-website-0.1.0.0...
   > [1 of 2] Compiling Papers           ( Papers.hs, dist-newstyle/build/x86_64-linux/ghc-9.6.7/types2026-website-0.1.0.0/x/site2/build/site2/site2-tmp/Papers.o )
   > [2 of 2] Compiling Main             ( Site.hs, dist-newstyle/build/x86_64-linux/ghc-9.6.7/types2026-website-0.1.0.0/x/site2/build/site2/site2-tmp/Main.o )
   > [3 of 3] Linking dist-newstyle/build/x86_64-linux/ghc-9.6.7/types2026-website-0.1.0.0/x/site2/build/site2/site2
   > Rebuilding site.
   > Initialising...
   >   Creating store...
   >   Creating provider...
   >   Running rules...
   > Checking for out-of-date items
   > Compiling
   >   Using async runtime with 1 threads...
   >   updated README.md
   [...]
   > Success
   > Deploying...
   > Requesting Kerberos ticket...
   > Deploying files to web server...
   > sending incremental file list
   >
   > sent 77,579 bytes  received 27 bytes  51,737.33 bytes/sec
   > total size is 68,180,272  speedup is 878.54
   > Source directory: /home/types-2026/website/tracking-repo/_site
   ```

   The build runs in a container, but the cabal state and build products are cached in the tracking repository (non-tracked subdirectories `_cabal` and `dist-newstyle`).
   So successive updates are fast.

6. Whenever a user pushes to the branch configured in the previous step, the reference-transaction hook takes care of deploying to the web server.
   The user can follow the progress of the build in their terminal:

   ```shell
   ~/types-web$ git commit --allow-empty --message 'Force deploy.' && git push
   [main 09eb263] Force deploy.
   Enumerating objects: 1, done.
   Counting objects: 100% (1/1), done.
   Writing objects: 100% (1/1), 197 bytes | 197.00 KiB/s, done.
   Total 1 (delta 0), reused 0 (delta 0), pack-reused 0 (from 0)
   > Pulling commit 09eb263720b44fabacf02f32430801a39b612c98...
   > Container id: 7da0c1e1f61807ff8b1236bbd8d3a4b202a0ceda9789cc6ec4e7b7824dfd3a7f
   > Building and running Hakyll...
   > Initialising...
   >   Creating store...
   >   Creating provider...
   >   Running rules...
   > Checking for out-of-date items
   > Compiling
   >   Using async runtime with 1 threads...
   > Success
   > Deploying...
   > Requesting Kerberos ticket...
   > Deploying files to web server...
   > sending incremental file list
   >
   > sent 77,579 bytes  received 27 bytes  155,212.00 bytes/sec
   > total size is 68,180,272  speedup is 878.54
   > Source directory: /home/types-2026/website/tracking-repo/_site
   To labs:website
   5daa65e..09eb263  main -> main
   ```

   If the build or deployment fails, the push will not go through.

   **Note:**
   The locking mechanism of git in the remote repository ensures that pushes cannot simultaneously.
   If one push is in progress, other pushes will fail.
   This can happen for long pushes that require Cabal rebuilding (for example, if a dependency is added to the Cabal file).

   **Note:**
   The reference-transaction hook continues to run even if a user aborts their remote push.
   To abort a long-running build, it has to be killed on the server.
