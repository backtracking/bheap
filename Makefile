all:
	obuild configure
	obuild build lib-bheap

doc:
	mkdir -p doc
	ocamldoc -d doc/ -html binary_heap.mli

install: all
	obuild install

uninstall:
	ocamlfind -remove bheap

clean:
	obuild clean
	rm -rf doc
