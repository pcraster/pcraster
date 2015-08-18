/* mboth.c */
extern ATTRIBUTES *currAttr;
extern const char *itemNames[12];
extern const int itemNamesLen;
extern const char *AttrStr(const ATTRIBUTES *a, ATTR_NRS i);
extern void GetAttrDouble(double *v, const ATTRIBUTES *a, ATTR_NRS i);
extern void SetAttrDouble(ATTRIBUTES *a, const double *v, ATTR_NRS i);
extern int EditItem(double *editValue, char *editString, ATTR_NRS i, int yStart, int xStart);
