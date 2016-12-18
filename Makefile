
%.beam: %.erl
	erlc $<

demo1: demo1.beam common.beam config.beam qc.beam

bench: bench.beam common.beam config.beam qc.beam tman.beam
