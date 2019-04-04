# Jenkins Quickstart

There are two ways to run the LCM test suite on Jenkins, as a <a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_Spock/" target="_blank">single</a> (i.e., one-off) run or a <a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_LCM_Matrix/" target="_blank">matrix</a> of combinations.

## Single run

Go to <a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_Spock/" target="_blank">http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_Spock/</a>, and from the navigation bar at the upper left, go to [Build with Parameters](http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_Spock/build). Here, you will see a form that has been pre-filled with some defaults. You could hit the "Build" button at the bottom, and it would run the suite without further ado, but in most cases you will want to specify some criteria before running the suite.

The following environment variables are available for customizing the run.

### `OPSC_BRANCH`
This denotes the branch of the <a href="https://github.com/riptano/ripcord/" target="_blank">ripcord</a> repository (where the OpsCenter code lives) to be used for the OpsCenter instance that you want to test. The suite will install a package version (deb or rpm), and assumes that the package will be available in the respective QA branch repository, which is automatically created by running the <a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_Branch_Build/" target="_blank">OpsCenter Branch Build</a> job.

### `BUILD_OPSC`
This checkbox is checked by default, and denotes that the <a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_Branch_Build/" target="_blank">OpsCenter Branch Build</a> job should run, building both deb and rpm packages, before this job runs, ensuring that the packages required to install the specified branch are available to the test suite.

### `AFT_OVERRIDE_BRANCH`
The code for the test suite itself (a.k.a., the AFTs, which stands for Automated Functional Tests) also lives in the <a href="https://github.com/riptano/ripcord/" target="_blank">ripcord</a> repository, specifically <a href="https://github.com/riptano/ripcord/tree/master/spock/afts/q" target="_blank">here</a>. The default is blank, which indicates to the Jenkins runner that you want to use the same value as in `OPSC_BRANCH`, which should contain the test code required to run that branch (since it will be based off of a stable branch containing the corresponding test suite). However, if you're worried that you don't have the latest test code in your branch, you can choose to:

1. rebase your branch so that you have the latest changes (recommended)
1. use the stable branch the OpsCenter branch that you're testing is based on (quick and dirty/sanity check)

### `TEST_NAME`
This drop-down denotes the module to use as the entry-point for the run.

* `lcm/test/nightly` (default) is a "vanilla" run that covers a lot of ground, but tries to focus on the most common user scenarios
* `lcm/initialize-with-topology` is a good choice if you just want to create the OpsCenter instance and the target cluster, but forgo running any LCM jobs. This module will handle all of the node creation and insert the appropriate values into the LCM model for you, including entries for config-profiles, repositories, and machine credentials. Upon completion of this job, you can tweak the resources manually and/or just run the job from the LCM UI.
* `lcm/initialize` will do everything `lcm/initialize-with-topology` does except create the target nodes and post them to the model. So it leaves you with a running OpsCenter instance with which you can add your own cluster manually et al. from the LCM UI.

When using the `lcm/initialize.*` modules, be sure to set the value to `persist` in `OPSC_EOL` and/or `TARGET_EOL`, as the default behavior is to clean up (i.e., destroy) the nodes at the end of the run.

### `OPSC_EOL` and `TARGET_EOL`
These drop-downs denote whether and/or under what conditions to destroy the OpsCenter node and target nodes (i.e., the DSE cluster), respectively.

* `destroy-on-exit` (default) means that the node(s) will be destroyed at the end of the run
* `persist-on-failure` tells the suite not to destroy the node(s) if any failure is reported at any time in the run
* `persist` tells the suite not to destroy the node(s) regardless of what happens

### `LOG_LEVEL`
This denotes the log level you wish to see in the Jenkins console. This only affects the log level for the test suite itself, which is what appears in the Jenkins console.

### `FLAX_SEED`
Intended for intermediate/advanced usage, and can always safely be left blank. The `FLAX_SEED` is a long integer used to seed the pseudo-random number generator, which the suite uses to vary certain parameters in a manner similar to "choose your own adventure." The `FLAX_SEED` can be left blank, which will allow the test framework to generate a random seed. If you want to rerun a particular configuration and compare the results, the seed will be printed in the previous run's console output; you can copy that and paste it into the `FLAX_SEED` field to get the suite to make the same "choices" it made in the earlier run. The default behavior of the suite is to use a current UNIX timestamp (in milliseconds—the same output as `(new java.util.Date()).getTime()`).

### `CUSTOM_ENV_FILE` and `CUSTOM_ENV`
Intended for advanced usage, and can always safely be left blank. The `CUSTOM_ENV_FILE` denotes a full path to a file containing YAML customizations that will alter the test configuration. The `CUSTOM_ENV` textarea is similar, but instead of pointing to a file, allows you to simply type the would-be contents of a file inline.

> If both `CUSTOM_ENV_FILE` and `CUSTOM_ENV` are present, note that conflicting entries in `CUSTOM_ENV` will override the entries in `CUSTOM_ENV_FILE`, but otherwise it is safe to use them in concert. 

### `Notes`
The notes section allows you to jot down anything down that helps clarify the intend of the run. Its value isn't used in any part of the suite.

### `email_recipients`
This denotes a space-delimited list of email addresses that will be notified upon completion of the run.

## Matrix

Intended mainly for advanced usage, the matrix job (<a href="http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_LCM_Matrix/" target="_blank">http://jenkins2.datastax.lan:8080/job/OpsCenter_AFT_LCM_Matrix/</a>) runs a multidimensional matrix of configuration combinations. The dimensions are fully customizable. It uses a special predefined `MATRIX_CONFIG` field that allows the user to customize what dimensions should be used and their corresponding values. Otherwise the fields are identicle to the above and apply in the exact same way. The default `MATRIX_CONFIG` doesn't require additional customizing in order to be useful, so you can leave it the way it is and just run the test.

> Note that the matrix uses a lot of hardware resources and shouldn't be run more than one at a time unless you know for sure that you have enough quota. Jenkins does not prevent this in any way. There _are_ configurations where it might be appropriate to run multiple concurrent instances of this job, but they are exceptional and require specialized knowledge beyond the scope of this quickstart.

## Displaying YAML in Jenkins `textarea`s

The following bookmarklet will help to format Jenkins `textarea`s so that the YAML displays more like a text editor (i.e., monospaced):

```javascript
javascript:(function(){Array.from(document.querySelectorAll('textarea')).map(x=>x.style.fontFamily='monospace');})()
```

