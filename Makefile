all:

check-gauche:
	gosh -I. test

clean:
	rm -f srfi-181-test.log *~ srfi/*~

