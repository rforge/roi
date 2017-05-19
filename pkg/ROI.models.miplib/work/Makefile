all: miplib2010-benchmark
	R --vanilla < ./read_mps_files.R

miplib2010-benchmark:
	wget http://miplib.zib.de/download/miplib2010-benchmark.tgz
	tar xzf miplib2010-benchmark.tgz
	rm -f miplib2010-benchmark.tgz
	gunzip miplib2010-benchmark/*

clean:
	rm -fr miplib2010-benchmark
