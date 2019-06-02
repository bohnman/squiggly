package com.github.bohnman.squiggly.environment;

import com.github.bohnman.core.cache.CoreCacheBuilderSpec;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.property.SquigglyPropertySource;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.util.Collections;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import static java.util.Objects.requireNonNull;

/**
 * Provides access to various configuration values that the Squiggly library uses.
 * <p>
 * Users can override the default configuration by putting a squiggly.properties in their classpath.
 */
@ThreadSafe
public class SquigglyEnvironmentOld {

    private final SortedMap<String, String> properties;
    private final SortedMap<String, String> origins;

    private final boolean filterImplicitlyIncludeBaseFields;
    private final boolean filterImplicitlyIncludeBaseFieldsInView;
    private final CoreCacheBuilderSpec filterPathCacheSpec;
    private final boolean filterPropagateViewToNestedFilters;
    private final Boolean appendContextInNodeFilter;
    private final CoreCacheBuilderSpec parserNodeCacheSpec;
    private final boolean propertyAddNonAnnotatedFieldsToBaseView;
    private final CoreCacheBuilderSpec propertyDescriptorCacheSpec;
    private final CoreCacheBuilderSpec convertCacheSpec;
    private final SquigglyPropertySource propertySource;
    private final String filterRequestParam;
    private final int maxArrayRangeDeclarationLength;


    /**
     * Initialize the config with 0 or more config sources.
     *
     * @param sources the config sources
     * @see SquigglyPropertySource
     */
    public SquigglyEnvironmentOld(SquigglyPropertySource propertySource) {
        this.propertySource = requireNonNull(propertySource);

        SortedMap<String, String> properties = new TreeMap<>();
        SortedMap<String, String> origins = new TreeMap<>();

        convertCacheSpec = getCacheSpec(this.propertySource, properties, origins, "squiggly.convert.cache.spec");
        filterImplicitlyIncludeBaseFields = getBool(this.propertySource, properties, origins, "squiggly.filter.implicitly-include-base-fields");
        filterImplicitlyIncludeBaseFieldsInView = getBool(this.propertySource, properties, origins, "squiggly.filter.implicitly-include-base-fields-in-view");
        filterPathCacheSpec = getCacheSpec(this.propertySource, properties, origins, "squiggly.filter.path-cache.spec");
        filterPropagateViewToNestedFilters = getBool(this.propertySource, properties, origins, "squiggly.filter.propagate-view-to-nested-filters");
        parserNodeCacheSpec = getCacheSpec(this.propertySource, properties, origins, "squiggly.parser.filter-cache.spec");
        propertyAddNonAnnotatedFieldsToBaseView = getBool(this.propertySource, properties, origins, "squiggly.property.add-non-annotated-fields-to-base-view");
        propertyDescriptorCacheSpec = getCacheSpec(this.propertySource, properties, origins, "squiggly.property.descriptor-cache.spec");
        filterRequestParam = getString(this.propertySource, properties, origins, "squiggly.filter.request.param");
        appendContextInNodeFilter = getBool(this.propertySource, properties, origins, "squiggly.filter.node.append-context");
        maxArrayRangeDeclarationLength = getInt(this.propertySource, properties, origins, "squiggly.function.array-range-declaration.max-length");

        this.properties = Collections.unmodifiableSortedMap(properties);
        this.origins = Collections.unmodifiableSortedMap(origins);
    }

    /**
     * Retrieve the value of the specified key as a String or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public String getString(String key) {
        return getString(key, null);
    }

    /**
     * Retrieve the value of the specified key as a String or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public String getString(String key, @Nullable String defaultValue) {

        if (key == null) {
            return defaultValue;
        }

        String value = properties.get(key);

        if (value == null) {
            value = propertySource.findByName(key);
        }

        return CoreObjects.firstNonNull(value, defaultValue);
    }

    /**
     * Retrieve the value of the specified key as a Boolean or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public Boolean getBoolean(String key) {
        return getBoolean(key, null);
    }

    /**
     * Retrieve the value of the specified key as a Boolean or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public Boolean getBoolean(String key, @Nullable Boolean defaultValue) {
        String value = properties.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toBoolean(value);
    }

    /**
     * Retrieve the value of the specified key as an Integer or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public Integer getInt(String key) {
        return getInt(key, null);
    }

    /**
     * Retrieve the value of the specified key as an Integer or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public Integer getInt(String key, @Nullable Integer defaultValue) {
        String value = properties.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).intValue();
    }

    /**
     * Retrieve the value of the specified key as a Long or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public Long getLong(String key) {
        return getLong(key, null);
    }

    /**
     * Retrieve the value of the specified key as a Long or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public Long getLong(String key, @Nullable Long defaultValue) {
        String value = properties.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).longValue();
    }

    /**
     * Retrieve the value of the specified key as a Float or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public Float getFloat(String key) {
        return getFloat(key, null);
    }

    /**
     * Retrieve the value of the specified key as a Float or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public Float getFloat(String key, @Nullable Float defaultValue) {
        String value = properties.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).floatValue();
    }

    /**
     * Retrieve the value of the specified key as a Double or null if not found.
     *
     * @param key the key
     * @return value or null
     */
    @Nullable
    public Double getDouble(String key) {
        return getDouble(key, null);
    }

    /**
     * Retrieve the value of the specified key as a Double or defaultValue if not found.
     *
     * @param key          the key
     * @param defaultValue the default value
     * @return value or defaultValue
     */
    @Nullable
    public Double getDouble(String key, @Nullable Double defaultValue) {
        String value = properties.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).doubleValue();
    }

    /**
     * Gets the convert cache specification.
     *
     * @return convert cache specification
     */
    public CoreCacheBuilderSpec getConvertCacheSpec() {
        return convertCacheSpec;
    }

    /**
     * Determines whether or not to include base fields for nested objects
     *
     * @return true if includes, false if not
     * @see com.github.bohnman.squiggly.view.PropertyView
     */
    public boolean isFilterImplicitlyIncludeBaseFields() {
        return filterImplicitlyIncludeBaseFields;
    }

    /**
     * Determines whether or not filters that specify a view also include "base" fields.
     *
     * @return true if includes, false if not
     */
    public boolean isFilterImplicitlyIncludeBaseFieldsInView() {
        return filterImplicitlyIncludeBaseFieldsInView;
    }

    /**
     * Get the query parameter for web-based filters
     *
     * @return query param
     */
    public String getFilterRequestParam() {
        return filterRequestParam;
    }

    /**
     * Get the {@link CoreCacheBuilderSpec} of the path cache in the squiggly filter.
     *
     * @return spec
     */
    public CoreCacheBuilderSpec getFilterPathCacheSpec() {
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
    public boolean isFilterPropagateViewToNestedFilters() {
        return filterPropagateViewToNestedFilters;
    }

    /**
     * Get the {@link CoreCacheBuilderSpec} of the node cache in the squiggly parser.
     *
     * @return spec
     * @see com.github.bohnman.squiggly.parse.SquigglyParser
     */
    public CoreCacheBuilderSpec getParserNodeCacheSpec() {
        return parserNodeCacheSpec;
    }

    /**
     * Determines whether or not non-annotated fields are added to the "base" view.
     *
     * @return true/false
     */
    public boolean isPropertyAddNonAnnotatedFieldsToBaseView() {
        return propertyAddNonAnnotatedFieldsToBaseView;
    }

    /**
     * Get the {@link CoreCacheBuilderSpec} of the descriptor cache in the property view introspector.
     *
     * @return spec
     */
    public CoreCacheBuilderSpec getPropertyDescriptorCacheSpec() {
        return propertyDescriptorCacheSpec;
    }

    /**
     * Determines whether to use the SquigglyContext in the node filter as last filter.
     *
     * @return true if use, false if not, or null if defer
     */
    public Boolean getAppendContextInNodeFilter() {
        return appendContextInNodeFilter;
    }

    /**
     * Gets the max length of an array range to avoid memory pollution.
     *
     * @return max length
     */
    public int getMaxArrayRangeDeclarationLength() {
        return maxArrayRangeDeclarationLength;
    }

    /**
     * Gets all the config as a map.
     *
     * @return map
     */
    public SortedMap<String, String> asMap() {
        return properties;
    }

    /**
     * Gets a map of all the config keys and whose values are the location where that key was read from.
     *
     * @return source map
     */
    public SortedMap<String, String> asLocationMap() {
        return origins;
    }

    private CoreCacheBuilderSpec getCacheSpec(SquigglyPropertySource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        if (value == null) {
            value = "";
        }

        return CoreCacheBuilderSpec.parse(value);
    }

    private Boolean getBool(SquigglyPropertySource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        if (CoreStrings.isEmpty(value)) {
            return null;
        }

        return "true".equals(value);
    }

    private String getString(SquigglyPropertySource source, Map<String, String> props, Map<String, String> locations, String key) {
        String property = source.findByName(key);
        String location = source.findOriginByName(key);
        props.put(key, property);
        locations.put(key, location);
        return property;
    }

    private int getInt(SquigglyPropertySource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Unable to convert " + value + " to int for key " + key);
        }
    }
}
