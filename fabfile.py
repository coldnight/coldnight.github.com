from fabric.api import sudo, cd, settings, env, task

env.use_ssh_config = True

repo_uri = "https://github.com/coldnight/coldnight.github.com"
dest_path = "/srv/coldnight.github.com"


@task
def deploy():
    """Deploy to serve"""

    with settings(warn_only=True):
        if sudo("test -d %s" % dest_path).failed:
            sudo("git clone  %s %s" % (repo_uri, dest_path))

        if sudo("virtualenv --version").failed:
            sudo("pip install -U virtualenv")

    with cd(dest_path):
        sudo("git fetch origin")
        sudo("git checkout master && git merge origin/master")
        sudo("git submodule init && git submodule update --remote --merge")
        with settings(warn_only=True):
            if sudo("test -d .venv").failed:
                sudo("virtualenv .venv")

        commands = [
            "source .venv/bin/activate",
            "pip install -U pelican Markdown",
            "make html"
        ]

        sudo(" && ".join(commands))
