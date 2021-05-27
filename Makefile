# gitmanager Manage git repos
# See LICENSE file for copyright and license details.

include config.mk

SRC = git_manager.sh

install: all
	mkdir -p ${DESTDIR}${PREFIX}/bin
	cp -f ${SRC} ${DESTDIR}${PREFIX}/bin/gitmanager
	chmod 755 ${DESTDIR}${PREFIX}/bin/gitmanager
	mkdir -p ${DESTDIR}${MANPREFIX}/man1
	sed "s/VERSION/${VERSION}/g" < gitmanager.1 > ${DESTDIR}${MANPREFIX}/man1/gitmanager.1
	chmod 644 ${DESTDIR}${MANPREFIX}/man1/gitmanager.1

uninstall:
	rm -f ${DESTDIR}${PREFIX}/bin/gitmanager\
		${DESTDIR}${MANPREFIX}/man1/gitmanager.1

.PHONY: all install uninstall
