#ifndef INCLUDED_CALC_INDEXCONTAINER
#define INCLUDED_CALC_INDEXCONTAINER

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

namespace calc {

class IndexParameter;
class ArrayDefinition;

//! user symbols that contain a set of index parameters
class IndexContainer {
	const ArrayDefinition* d_partOf;
public:
	typedef std::set<const IndexParameter *>Set;
protected:
	virtual void addToSet(Set& setToBeAddedTo)const=0;
public:
	IndexContainer(const ArrayDefinition* partOf);

	//! add to active indices, if IsOn
	void addActiveToSet(Set& setToBeAddedTo)const;

	//! is this index set active?
	virtual bool isOn()const=0;

	//! to array definition belongs this index?
	const ArrayDefinition* partOf() const; 
};

}

#endif
