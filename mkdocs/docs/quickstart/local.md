# Quickstart: local development environment

When developing OpsCenter/LCM features on a branch, it is often more convenient to run the AFT suite against a local OpsCenter instance, rather than pushing code to origin, running a build job, etc. This is also the only way available to run the suite against LXD containerized nodes, which greatly decreases runtime, and thus is more suitable for iterative development.

There are two ways to run the AFT suite, as a single (i.e., one-off) run or a matrix of combinations. While it is definitely possible to run the matrix locally, it is only practical to do that against cloud resources. For local development purposes, the much more practical use-case would be to run in "single" mode. So this quickstart documentation will focus on running against an existing OpsCenter instance running locally.

## Dependencies

### Ctool, Python, virtualenv

> N.B. it is assumed that you have a working automaton config file in an acceptible location and a reasonable familiarity with <a href="https://github.com/riptano/ctool" target="_blank">ctool</a>. See <a href="https://datastax.jira.com/wiki/spaces/QA/pages/61014295/Install+and+setup+ctool" target="_blank">this guide</a> for tips. It is also a good idea to use a <a href="https://virtualenv.pypa.io/en/stable/" target="_blank">virtualenv</a>. Ctool requires Python2, and this project requires the latest ctool for access to <a href="http://openstack.datastax.lan/" target="_blank">openstack</a> and other cloud computing resources. A VPN connection is also required to work with ctool/openstack and to download from most repositories. It is also assumed that you have a reasonable familiarity with YAML syntax.

To get started, cd into the project directory, activate your virtualenv and install Python dependencies:

```shell
cd ripcord/spock/afts/q/qapi
. ~/.venv/your-venv/bin/activate
pip install -r requirements.txt --upgrade
```

The `--upgrade` is useful here because ctool, one of the dependencies, is obtained via git on its `master` branch. 

### Connectors

There are currently three connectors at the project root:

1. `linen-ctool-connector` — used for launching/connecting to cloud resources, such as openstack, rightscale, etc.
2. `linen-lxd-connector` — used for launching/connecting to LXD containers locally
3. `linen-ssh-connector` — used for connecting to (i.e., and not launching) local instances (such as OpsCenter)

The default connector used by the framework is the `linen-ctool-connector`. However, these connectors can be mixed into a single run, such that the OpsCenter node can connect via ssh locally while the target nodes launch/connect to openstack or LXD nodes.

The connectors are their own leiningen projects. Building them and installing them locally is required for their use. To build and install them all, you could do the following:

```shell
for c in ctool lxd ssh; do cd ./linen-${c}-connector; lein install; cd ..; done
```

## Configuration

A little bit of configuration is required in order to make use of a running OpsCenter node. Find the ssh-connector template at <a href="https://github.com/riptano/ripcord/blob/master/spock/afts/q/qapi/resources/linen/templates/ssh-connector.yaml" target="_blank">`resources/linen/templates/ssh-connector.yaml`</a>. Copy this file for editing outside of the repo, e.g., 

```shell
$ cp resources/linen/templates/ssh-connector.yaml ~/ssh-connector.yaml
```

The contents will be something like the following

```yaml
OPSC.node.connector:
  name: linen-ssh-connector.core
  options:
    #ssh-user: linen
    #ip-address: 10.166.115.157
    #private-key: |-
    #  -----BEGIN RSA PRIVATE KEY-----
    #  ...
    #  ...
    #  -----END RSA PRIVATE KEY-----

    ssh-user: &ssh-user ~$ whoami
    ip-address: ~$ ip addr show tun0 | sed '3q;d' | awk '{ print $2 }' | sed 's/.\{3\}$//'
    private-key: ~$ cat ~/.ssh/id_rsa

OPSC.node.users.0:
  username: *ssh-user
  private-key-file: ~{HOME}/.ssh/id_rsa

OPSC.pcap.enabled: false
OPSC.vmstat.enabled: false

OPSC.log.sources: []

SKIP.healthcheck.opscenter: true
```

Under `options`, the defaults there might be good enough to get the right values in place, but it's also possible to simply hard-code the values here, too, as in the commented out portion of `options` above. These values correspond to an actual ssh-user in your local environment. The framework uses ssh to connect to the localhost with these credentials, and also provides the ip address to the target nodes for a callback address, so this address cannot be "localhost" or "127.0.0.1", etc.

> N.B. Wherever you see `~$` and a shell command in the framework's YAML config files, this is a short-hand provided by the framework to allow you to use the stdout of the process created by the shell command as a value.

It's also important to make sure that the `username` under `OPSC.node.users.0` exists. It doesn't have to be the same user as above, but it's not necessarily a bad idea. 

Once you have this template set up correctly the first time, you can reuse it without further ado on subsequent runs.

The following command will run the suite with your local OpsCenter instance:


```shell
$ lein run -- -c config/jenkins.yaml -m lcm/test/nightly ~/ssh-connector.yaml
```

The CLI expects values for config (`-c`) and module (`-m`), and a variadic list of YAML configuration files to reduce/evaluate/merge into one another, such that the last file listed overrides the one before it. So if you have other customizations, you can append them to the argument vector, such as:

```shell
$ lein run -- -c config/jenkins.yaml -m lcm/test/nightly ~/ssh-connector.yaml ~/lcm-workspace.yaml
```

