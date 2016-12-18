# QuickChord
quickchord can jump-start Chord by using T-Man or just run Chord without improvement. This code written in Erlang will run on TEDA.


## Deployment

### TEDA preparation
Do the necaissairy preperations for TEDA as discribed in the TEDA-Memento:

Deploy RSA key:

    ../../scripts/RSA_setup.sh hosts.conf 

Create hosts_alive.conf

    ../../scripts/ping.sh quickchord hosts.conf 

Deploy App:

    ../../scripts/depl_app.sh quickchord make

Deploy Erlang nodes:

    ../../scripts/depl_enodes.sh quickchord

Run master():

    ../../scripts/run.sh quickchord "quickchord:master()" hosts_alive.conf diufpc80.unifr.ch UserName

where:

 - UserName is the unifr username (ex. ZieglerF)
 - diufpc80.unifr.ch can be one of the hosts in hosts_alive.conf

### Run Demo1

#### Locally
To run demo1 locally make sure that T is set to 3(to get same output as in our report) in config.erl then run:

    make demo1
    erl -s demo1 master

#### Teda
Copy all .erl files into teda/apps/quickchord/ and run

    ../../scripts/run.sh quickchord "demo1:master()" hosts_alive.conf diufpc80.unifr.ch UserName

### Step 3: Starting Chord without T-Man


### Step 4: Starting Chord with T-Man

