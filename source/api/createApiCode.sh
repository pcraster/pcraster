set -e

RANCOM=rancom.tem
RANDEF=randef.tem
TESTAPI=testapi.tem

# as copied from Makefile in support of Scons build
echo "/* XX DO NOT EDIT: CREATED FROM $RANCOM */" > ranuint1.c
echo "#line 1 \"$RANCOM\"" >> ranuint1.c
sed -e s/THIS_TYPE_T/UINT1_T/g  -e s/THIS_TYPE/UINT1/g $RANCOM >> ranuint1.c
#ranint4.c: $RANCOM
echo "/* DO NOT EDIT: CREATED FROM $RANCOM */" > ranint4.c
echo "#line 1 \"$RANCOM\"" >> ranint4.c
sed -e s/THIS_TYPE_T/INT4/g  -e s/THIS_TYPE/INT4/g $RANCOM >> ranint4.c

#ranreal8.c: $RANCOM
echo "/* DO NOT EDIT: CREATED FROM $RANCOM */" > ranreal8.c
echo "#line 1 \"$RANCOM\"" >> ranreal8.c
sed -e s/THIS_TYPE_T/REAL8/g  -e s/THIS_TYPE/REAL8/g $RANCOM >> ranreal8.c
#api.h: $RANDEF randef.hh Makefile
cat randef.hh > api.h
echo "#line 1 \"$RANDEF\"" >> api.h
sed -e s/THIS_TYPE_T/UINT1_T/g  -e s/THIS_TYPE/UINT1/g $RANDEF >> api.h
echo "#line 1 \"$RANDEF\"" >> api.h
sed -e s/THIS_TYPE_T/INT4/g  -e s/THIS_TYPE/INT4/g $RANDEF >> api.h
echo "#line 1 \"$RANDEF\"" >> api.h
sed -e s/THIS_TYPE_T/REAL8/g  -e s/THIS_TYPE/REAL8/g $RANDEF >> api.h
echo "#ifdef __cplusplus" >> api.h
echo "}" >> api.h
echo "#endif" >> api.h
echo "#endif /* API__H */" >> api.h
#testapi.inc: $TESTAPI
echo "/* DO NOT EDIT: CREATED FROM $TESTAPI */" > testapi.inc
echo "#line 1 \"$TESTAPI\"" >> testapi.inc
sed 's/THIS_TYPE/UINT1/g' < $TESTAPI >> testapi.inc 
echo "#line 1 \"$TESTAPI\"" >> testapi.inc
sed 's/THIS_TYPE/INT4/g' < $TESTAPI >> testapi.inc 
echo "#line 1 \"$TESTAPI\"" >> testapi.inc
sed 's/THIS_TYPE/REAL8/g' < $TESTAPI >> testapi.inc 
