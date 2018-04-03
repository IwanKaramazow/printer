wadler_test:
	./node_modules/.bin/bsb -backend native && ./lib/bs/native/test.native

compat_test:
	./node_modules/.bin/bsb -backend native && ./lib/bs/native/compat.native

test_all:
	./node_modules/.bin/bsb -backend native && ./lib/bs/native/compat.native && ./lib/bs/native/test.native
