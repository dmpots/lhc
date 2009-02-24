import sys
import os
import os.path
import pysqlite2.dbapi2

dbpath = sys.argv[1]
dirs   = sys.argv[2:]

db = pysqlite2.dbapi2.connect(dbpath)

tcfields = db.execute("""SELECT DISTINCT
                           tc.value
                           FROM ticket t
                           LEFT OUTER JOIN ticket_custom tc ON
                                (t.id = tc.ticket AND tc.name = 'testcase')
                           WHERE status IN ('new', 'assigned', 'reopened')""")

tests = list(x for tc, in tcfields
               for x in tc.split())
# print tests

for d, (f, ext) in ((d2, os.path.splitext(f))
                      for d in dirs
                      for d2, _, files in os.walk(d)
                      for f in files):
    if ext in ['.hs', '.lhs']:
        if f not in tests:
            print 'warning: test %r not mentioned in any ticket\'s "Test Case" field' % os.path.join(d, f+ext)
