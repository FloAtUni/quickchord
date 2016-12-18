# QuickChord
quickchord can jump-start Chord by using T-Man or just run Chord without improvement. This code written in Erlang will run on TEDA.


## Deployment

### Step 1: TEDA preparation
Deploy RSA key:

    ../../scripts/RSA_setup.sh hosts.conf 

Create hosts_alive.conf

    ../../scripts/ping.sh quickchord hosts.conf 

Deploy App:

    ../../scripts/depl_app.sh quickchord make

Deploy Erlang nodes:

    ../../scripts/depl_enodes.sh quickchord

Run master():

    ../../scripts/run.sh quickchord "quickchord:master(??)" hosts_alive.conf diufpc80.unifr.ch UserName

where:

 - UserName is the unifr username (ex. ZieglerF)
 - diufpc80.unifr.ch can be one of the hosts in hosts_alive.conf


### Step 2: Starting Chord without T-Man


### Step 3: Starting Chord with T-Man

