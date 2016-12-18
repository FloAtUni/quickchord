
%.beam: %.erl
	erlc $<

demo1: demo1.beam common.beam config.beam qc.beam
