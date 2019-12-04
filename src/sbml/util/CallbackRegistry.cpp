#ifdef __cplusplus

#include <algorithm>

#include <sbml/common/operationReturnValues.h>
#include <sbml/util/CallbackRegistry.h>

LIBSBML_CPP_NAMESPACE_BEGIN

CallbackRegistry&
CallbackRegistry::getInstance()
{
	static CallbackRegistry singletonObj;
	return singletonObj;
}

int CallbackRegistry::invokeCallbacks(SBMLDocument* doc)
{
	int result = LIBSBML_OPERATION_SUCCESS;
	
	std::vector<Callback*>& cbs = getInstance().mCallbacks;
	int nMax = (int)cbs.size();
	for (int i = 0; i < nMax; ++i)
	{
		Callback* cb = cbs[(size_t)i];
		result += cb->process(doc);
	}

	return result;
}

void CallbackRegistry::clearCallbacks()
{
	std::vector<Callback*>& cbs = getInstance().mCallbacks;
	cbs.clear();
}

void CallbackRegistry::addCallback(Callback *cb)
{
	getInstance().mCallbacks.push_back(cb);
}

int CallbackRegistry::getNumCallbacks()
{
	return (int)getInstance().mCallbacks.size();
}

void CallbackRegistry::removeCallback(int index)
{
	if (index < 0 || index >= getNumCallbacks()) return;

	std::vector<Callback*>& cbs = getInstance().mCallbacks;
	cbs.erase(cbs.begin() + index, cbs.begin() + 1 + index);
}

void CallbackRegistry::removeCallback(Callback* cb)
{
  std::vector<Callback*>& cbs = getInstance().mCallbacks;

  std::vector<Callback*>::iterator it = std::find(cbs.begin(), cbs.end(), cb);
  if (it == cbs.end())
    return;

  cbs.erase(it);
}

CallbackRegistry::CallbackRegistry()
	: mCallbacks()
{
}


Callback::~Callback()
{
}

int Callback::process(SBMLDocument* doc)
{
	return LIBSBML_OPERATION_SUCCESS;
}

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
