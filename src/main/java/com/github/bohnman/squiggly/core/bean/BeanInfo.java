package com.github.bohnman.squiggly.core.bean;

import com.google.common.collect.ImmutableSet;

import java.util.Map;
import java.util.Set;

public class BeanInfo {

    private Map<String, Set<String>> viewNameToPropertiesNames;
    private Set<String> unwrappedProperties;

    public BeanInfo(Map<String, Set<String>> viewNameToPropertiesNames, Set<String> unwrappedProperties) {
        this.viewNameToPropertiesNames = viewNameToPropertiesNames;
        this.unwrappedProperties = unwrappedProperties;
    }

    public Set<String> getPropertyNamesForView(String view) {
        Set<String> properties = viewNameToPropertiesNames.get(view);

        if (properties == null) {
            properties = ImmutableSet.of();
        }

        return properties;
    }

    public boolean isUnwrapped(String property) {
        return unwrappedProperties.contains(property);
    }
}
