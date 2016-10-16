package com.github.bohnman.squiggly.config;

import net.jcip.annotations.ThreadSafe;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * Provides access to various configuration values that the Squiggly library uses.
 */
@ThreadSafe
public class SquigglyConfig {

    private static final boolean filterImplicitlyIncludeBaseFields;
    private static final int filterPathCacheMaxSize;
    private static final boolean filterPropagateViewToNestedFilters;

    private static final int parserNodeCacheMaxSize;

    private static final int propertyDescriptorCacheMaxSize;

    static {
        Map<String, String> props = new HashMap<>();
        loadProps(props, "squiggly.default.properties");
        loadProps(props, "squiggly.properties");

        filterImplicitlyIncludeBaseFields = getBool(props, "filter.implicitlyIncludeBaseFields");
        filterPathCacheMaxSize = getInt(props, "filter.pathCache.maxSize");
        filterPropagateViewToNestedFilters = getBool(props, "filter.propagateViewToNestedFilters");
        parserNodeCacheMaxSize = getInt(props, "parser.nodeCache.maxSize");
        propertyDescriptorCacheMaxSize = getInt(props, "property.descriptorCache.maxSize");
    }

    private static boolean getBool(Map<String, String> props, String key) {
        return "true".equals(props.get(key));
    }

    private static int getInt(Map<String, String> props, String key) {
        try {
            return Integer.parseInt(props.get(key));
        } catch (NumberFormatException e) {
            throw new RuntimeException("Unable to convert " + props.get(key) + " to int for key " + key);
        }
    }

    private static void loadProps(Map<String, String> props, String file) {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream(file);

        if (inputStream == null) {
            return;
        }

        Properties fileProps = new Properties();

        try {
            fileProps.load(inputStream);
        } catch (IOException e) {
            throw new RuntimeException("Unable to load properties from classpath resource " + file, e);
        }

        fileProps.forEach((key, val) -> props.put(key.toString(), val.toString()));
    }

    private SquigglyConfig() {
    }

    /**
     * Determines whether or not filters that specify a view also include "base" fields.
     *
     * @return true if includes, false if not
     * @see com.github.bohnman.squiggly.view.PropertyView
     */
    public static boolean isFilterImplicitlyIncludeBaseFields() {
        return filterImplicitlyIncludeBaseFields;
    }

    /**
     * Get the max size of the path cache in the squiggly filter.  Use -1 to specify infinite caching or 0 to specify
     * no caching.
     *
     * @return max size
     * @see com.github.bohnman.squiggly.filter.SquigglyPropertyFilter
     */
    public static int getFilterPathCacheMaxSize() {
        return filterPathCacheMaxSize;
    }

    /**
     * Determines whether or not filters that specify a view also propagtes that view to nested filters.
     *
     * For example, given a view called "full", does the full view also apply to the nested objects or does the nested
     * object only include base fields.
     *
     * @return true if includes, false if not
     *
     */
    public static boolean isFilterPropagateViewToNestedFilters() {
        return filterPropagateViewToNestedFilters;
    }

    /**
     * Get the max size of the node cache in the squiggly parser.  Use -1 to specify infinite caching or 0 to specify
     * no caching.
     *
     * @return max size
     * @see com.github.bohnman.squiggly.parser.SquigglyParser
     */
    public static int getParserNodeCacheMaxSize() {
        return parserNodeCacheMaxSize;
    }

    /**
     * Get the max size of the descriptor cache in the property view introspector.  Use -1 to specify infinite caching
     * or 0 to specify no caching.
     *
     * @return max size
     * @see com.github.bohnman.squiggly.view.PropertyViewIntrospector
     */
    public static int getPropertyDescriptorCacheMaxSize() {
        return propertyDescriptorCacheMaxSize;
    }
}
