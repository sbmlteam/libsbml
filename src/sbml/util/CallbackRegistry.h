#ifndef CallbackRegistry_h
#define CallbackRegistry_h

#ifdef __cplusplus

#include <vector>

#include <sbml/common/extern.h>
#include <sbml/common/libsbml-namespace.h>

LIBSBML_CPP_NAMESPACE_BEGIN

class SBMLDocument;

class LIBSBML_EXTERN Callback
{
public:

  virtual ~Callback();

	/**
	 * The processing function to be implemented by the callback
	 * 
	 * In order to stop processing return a value other than 
	 * LIBSBML_OPERATION_SUCCESS. 
	 * 
	 * @return LIBSBML_OPERATION_SUCCESS to continue the operations
	 *         any other value to stop
	 */
	virtual int process(SBMLDocument* doc);
};


class LIBSBML_EXTERN CallbackRegistry
{
public:

	/**
	 * Invokes all registered callbacks on the given document. If the callbacks indicate
	 * that processing should be stopped they return a value other than operation success.
	 * 
	 * @return LIBSBML_OPERATION_SUCCESS to indicate that processing should be continued, 
	 *         any other value to stop processing
	 */
	static int invokeCallbacks(SBMLDocument* doc);

  /**
   * Clears all registered processing callbacks
   */
	static void clearCallbacks();
	/**
   * Registers a new processing callback that will be called with a newly instantiated
   * ModelDefinition object. This allows for all post processing on it that needs to
   * happen before integrating it with the target document.
   *
   * @param cb the callback.
   */
	static void addCallback(Callback *cb);

	/**
	 * @return the number of registered callbacks.
	 */
	static int getNumCallbacks();

	/**
   * Removes the callback with given index.
   *
   * @param index the index of the callback to be removed from the list.
   *
   */
	static void removeCallback(int index);


	/**
	 * Removes the specified callback from the list of registered callbacks
	 *
	 * @param cb the callback to be removed.
	 */
	static void removeCallback(Callback *cb);


protected:

	/**
	 * the static instance of the class
	 */
	static CallbackRegistry& getInstance();

	/**
   * protected constructor, use the static methods for access.
   */
	CallbackRegistry();

#ifndef SWIG
	std::vector<Callback*> mCallbacks;
#endif

};

LIBSBML_CPP_NAMESPACE_END


#endif /* __cplusplus */
#endif /* CallbackRegistry_h */