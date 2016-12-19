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
Make sure that T is set to 3 in config.erl.
Copy all .erl files into teda/apps/quickchord/ and run

    ../../scripts/run.sh quickchord "demo1_teda:master()" hosts_alive.conf diufpc80.unifr.ch UserName

### Run Benchmark

#### Locally
To run the benchmark locally set T in config.erl to the desired value then run:

    make bench
    erl -s bench master

#### Teda
Make sure that T is set to 3 in config.erl and run.

    make deploy
    ../../scripts/run.sh quickchord "bench_teda:master()" hosts_alive.conf diufpc80.unifr.ch UserName
