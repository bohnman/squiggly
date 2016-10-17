package com.github.bohnman.squiggly.config;

import com.google.common.cache.CacheBuilderSpec;
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
    private static final CacheBuilderSpec filterPathCacheSpec;
    private static final boolean filterPropagateViewToNestedFilters;

    private static final CacheBuilderSpec parserNodeCacheSpec;

    private static boolean propertyAddNonAnnotatedFieldsToBaseView;
    private static final CacheBuilderSpec propertyDescriptorCacheSpec;

    static {
        Map<String, String> props = new HashMap<>();
        loadProps(props, "squiggly.default.properties");
        loadProps(props, "squiggly.properties");

        filterImplicitlyIncludeBaseFields = getBool(props, "filter.implicitlyIncludeBaseFields");
        filterPathCacheSpec = getCacheSpec(props, "filter.pathCache.spec");
        filterPropagateViewToNestedFilters = getBool(props, "filter.propagateViewToNestedFilters");
        parserNodeCacheSpec = getCacheSpec(props, "parser.nodeCache.spec");
        propertyAddNonAnnotatedFieldsToBaseView = getBool(props, "property.addNonAnnotatedFieldsToBaseView");
        propertyDescriptorCacheSpec = getCacheSpec(props, "property.descriptorCache.spec");
    }

    private static CacheBuilderSpec getCacheSpec(Map<String, String> props, String key) {
        String value = props.get(key);

        if (value == null) {
            value = "";
        }

        return CacheBuilderSpec.parse(value);
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
     * Get the {@link CacheBuilderSpec} of the path cache in the squiggly filter.
     *
     * @return spec
     * @see com.github.bohnman.squiggly.filter.SquigglyPropertyFilter
     */
    public static CacheBuilderSpec getFilterPathCacheSpec() {
        return filterPathCacheSpec;
    }

    /**
     * Determines whether or not filters that specify a view also propagtes that view to nested filters.
     * <p>
     * For example, given a view called "full", does the full view also apply to the nested objects or does the nested
     * object only include base fields.
     *
     * @return true if includes, false if not
     */
    public static boolean isFilterPropagateViewToNestedFilters() {
        return filterPropagateViewToNestedFilters;
    }

    /**
     * Get the {@link CacheBuilderSpec} of the node cache in the squiggly parser.
     *
     * @return spec
     * @see com.github.bohnman.squiggly.parser.SquigglyParser
     */
    public static CacheBuilderSpec getParserNodeCacheSpec() {
        return parserNodeCacheSpec;
    }

    /**
     * Determines whether or not non-annotated fields are added to the "base" view.
     *
     * @return true/false
     * @see com.github.bohnman.squiggly.view.PropertyViewIntrospector
     */
    public static boolean isPropertyAddNonAnnotatedFieldsToBaseView() {
        return propertyAddNonAnnotatedFieldsToBaseView;
    }

    /**
     * Get the {@link CacheBuilderSpec} of the descriptor cache in the property view introspector.
     *
     * @return spec
     * @see com.github.bohnman.squiggly.view.PropertyViewIntrospector
     */
    public static CacheBuilderSpec getPropertyDescriptorCacheSpec() {
        return propertyDescriptorCacheSpec;
    }
}
