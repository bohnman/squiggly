package com.github.bohnman.squiggly.core.bean;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * Holds metadata about an introspected bean class.
 */
public class BeanInfo {

    private Map<String, Set<String>> viewNameToPropertiesNames;
    private Set<String> unwrappedProperties;

    public BeanInfo(Map<String, Set<String>> viewNameToPropertiesNames, Set<String> unwrappedProperties) {
        this.viewNameToPropertiesNames = viewNameToPropertiesNames;
        this.unwrappedProperties = unwrappedProperties;
    }

    /**
     * Get all the property names for the specified view name annotated with
     * {@link com.github.bohnman.squiggly.core.view.PropertyView}.
     *
     * @param view the view name
     * @return a set of properties.
     */
    public Set<String> getPropertyNamesForView(String view) {
        Set<String> properties = viewNameToPropertiesNames.get(view);

        if (properties == null) {
            properties = Collections.emptySet();
        }

        return properties;
    }

    /**
     * Indicates whether the specified property is "unwrapped".  Useful for the Jackson library.
     *
     * @param property property name
     * @return true/false
     */
    public boolean isUnwrapped(String property) {
        return unwrappedProperties.contains(property);
    }
}
