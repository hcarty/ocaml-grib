GRIB_API_VERSION = 1.9.16
OCAMLMAKEFILE = OCamlMakefile
ANNOTATE = yes
PACKS = batteries pcre
LIBS =
OCAMLLIBPATH=
INCDIRS=
LIBDIRS=
EXTLIBDIRS=
THREADS = yes

CLIBS = grib_api jasper png m
# We turn on debugger support in all our modules for now.
CFLAGS = -g

# Pack all of the modules together in a monolithic Grib module
RESULT = grib
LIB_PACK_NAME := grib
OCAMLFLAGS := -for-pack Grib

SOURCES = grib_stubs.c message.ml inventory.ml handle.ml index.ml iterator.ml multi.ml nearest.ml grib.mli

# Installation settings
LIBINSTALL_FILES = grib.mli grib.cmi grib.cma grib.cmxa *.a *.so

all: packed-cmi byte-code-library native-code-library

opt: packed-cmi native-code-library

packed-cmi:
	ocamlfind c -package batteries -thread grib.mli

mrproper: clean
	rm -f *~ *.cmi *.cmo *.top *.so

.PHONY: mrproper

install: libinstall

uninstall: libuninstall

tgz:
	git archive --format=tar --prefix=ocaml-grib-$(GRIB_API_VERSION)/ HEAD | gzip -n > ocaml-grib-$(GRIB_API_VERSION).tar.gz

include $(OCAMLMAKEFILE)
