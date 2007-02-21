#ifndef LAYOUT_TESTS_UTILITY_H__
#define LAYOUT_TESTS_UTILITY_H__

#include <xml/XMLNode.h>

/**
 * This function compares two XMLNodes if they have identical content.
 * If the two nodes are identical, the function returns true.
 */
bool compareXMLNodes(const XMLNode& node1,const XMLNode& node2);


#endif // LAYOUT_TESTS_UTILITY_H__
