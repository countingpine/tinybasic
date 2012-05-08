#ifndef __TBSTRING_H__
#define __TBSTRING_H__

#define strIsResult(s) (((s).flags) & 0x00000001)
#define strIsTemp(s)   (((s).flags) & 0x00000002)
#define strIsUsed(s)   (((s).flags) & 0x00000004)
#define strIsLocal(s)  (((s).flags) & 0x00000008)

#define strSetIsResult(s) (((s).flags) |= 0x00000001)
#define strSetIsTemp(s)   (((s).flags) |= 0x00000002)
#define strSetIsUsed(s)   (((s).flags) |= 0x00000004)
#define strSetIsLocal(s)  (((s).flags) |= 0x00000008)

struct TBSTRING {
	TBSTRING();
	TBSTRING(const TBSTRING&);
	~TBSTRING();
	int operator =(const TBSTRING&);

	void *data;
	int len;
	int size;

	TBSTRING *old_temp;

	int flags;
};

int TBSTRING_EQ(const TBSTRING& lhs, const TBSTRING& rhs);
int TBSTRING_NE(const TBSTRING& lhs, const TBSTRING& rhs);
void TBSTRING_assign(TBSTRING& lhs, const TBSTRING& rhs);
void TBSTRING_append(TBSTRING& lhs, const TBSTRING& rhs);

#endif
