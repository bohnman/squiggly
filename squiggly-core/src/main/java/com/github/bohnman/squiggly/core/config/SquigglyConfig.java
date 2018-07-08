package com.github.bohnman.squiggly.core.config;

import com.github.bohnman.core.cache.CoreCacheBuilderSpec;
import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.convert.CoreConversions;
import com.github.bohnman.core.lang.CoreObjects;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.config.source.CompositeConfigSource;
import com.github.bohnman.squiggly.core.config.source.PropertiesConfigSource;
import com.github.bohnman.squiggly.core.config.source.SquigglyConfigSource;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Provides access to various configuration values that the Squiggly library uses.
 * <p>
 * Users can override the default configuration by putting a squiggly.properties in their classpath.
 */
@ThreadSafe
public class SquigglyConfig {

    private final SortedMap<String, String> propsMap;
    private final SortedMap<String, String> locationMap;

    private final boolean filterImplicitlyIncludeBaseFields;
    private final boolean filterImplicitlyIncludeBaseFieldsInView;
    private final CoreCacheBuilderSpec filterPathCacheSpec;
    private final boolean filterPropagateViewToNestedFilters;
    private final Boolean appendContextInNodeFilter;
    private final CoreCacheBuilderSpec parserNodeCacheSpec;
    private final boolean propertyAddNonAnnotatedFieldsToBaseView;
    private final CoreCacheBuilderSpec propertyDescriptorCacheSpec;
    private final CoreCacheBuilderSpec convertCacheSpec;
    private final CompositeConfigSource source;
    private final String filterRequestParam;
    private final SquigglyEnvironment functionEnvironment;

    public SquigglyConfig(SquigglyConfigSource... sources) {
        this(Arrays.asList(sources));
    }

    public SquigglyConfig(Iterable<SquigglyConfigSource> sources) {
        Stream<SquigglyConfigSource> sourceStream = CoreStreams.of(sources);

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL squigglyProps = classLoader.getResource("squiggly.properties");
        URL squigglyDefaultProps = classLoader.getResource("squiggly.default.properties");

        if (squigglyProps != null) {
            sourceStream = Stream.concat(sourceStream, Stream.of(new PropertiesConfigSource(squigglyProps)));
        }

        if (squigglyDefaultProps != null) {
            sourceStream = Stream.concat(sourceStream, Stream.of(new PropertiesConfigSource(squigglyDefaultProps)));
        }

        this.source = new CompositeConfigSource(sourceStream.collect(Collectors.toList()));

        SortedMap<String, String> propsMap = new TreeMap<>();
        SortedMap<String, String> locationMap = new TreeMap<>();

        convertCacheSpec = getCacheSpec(source, propsMap, locationMap, "squiggly.convert.cache.spec");
        filterImplicitlyIncludeBaseFields = getBool(source, propsMap, locationMap, "squiggly.filter.implicitlyIncludeBaseFields");
        filterImplicitlyIncludeBaseFieldsInView = getBool(source, propsMap, locationMap, "squiggly.filter.implicitlyIncludeBaseFieldsInView");
        filterPathCacheSpec = getCacheSpec(source, propsMap, locationMap, "squiggly.filter.pathCache.spec");
        filterPropagateViewToNestedFilters = getBool(source, propsMap, locationMap, "squiggly.filter.propagateViewToNestedFilters");
        parserNodeCacheSpec = getCacheSpec(source, propsMap, locationMap, "squiggly.parser.nodeCache.spec");
        propertyAddNonAnnotatedFieldsToBaseView = getBool(source, propsMap, locationMap, "squiggly.property.addNonAnnotatedFieldsToBaseView");
        propertyDescriptorCacheSpec = getCacheSpec(source, propsMap, locationMap, "squiggly.property.descriptorCache.spec");
        filterRequestParam = getString(source, propsMap, locationMap, "squiggly.filter.request.param");
        appendContextInNodeFilter = getBool(source, propsMap, locationMap, "squiggly.filter.node.appendContext");
        functionEnvironment = SquigglyEnvironment.valueOf(getString(source, propsMap, locationMap, "squiggly.env").toUpperCase());

        this.propsMap = Collections.unmodifiableSortedMap(propsMap);
        this.locationMap = Collections.unmodifiableSortedMap(locationMap);
    }

    @Nullable
    public String getString(String key) {
        return getString(key, null);
    }

    @Nullable
    public String getString(String key, @Nullable String defaultValue) {

        if (key == null) {
            return defaultValue;
        }

        String value = propsMap.get(key);

        if (value == null) {
            value = source.getProperty(key);
        }

        return CoreObjects.firstNonNull(value, defaultValue);
    }

    @Nullable
    public Boolean getBoolean(String key) {
        return getBoolean(key, null);
    }

    @Nullable
    public Boolean getBoolean(String key, @Nullable Boolean defaultValue) {
        String value = propsMap.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toBoolean(value);
    }

    @Nullable
    public Integer getInt(String key) {
        return getInt(key, null);
    }

    @Nullable
    public Integer getInt(String key, @Nullable Integer defaultValue) {
        String value = propsMap.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).intValue();
    }


    @Nullable
    public Long getLong(String key) {
        return getLong(key, null);
    }

    @Nullable
    public Long getLong(String key, @Nullable Long defaultValue) {
        String value = propsMap.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).longValue();
    }


    @Nullable
    public Float getFloat(String key) {
        return getFloat(key, null);
    }

    @Nullable
    public Float getFloat(String key, @Nullable Float defaultValue) {
        String value = propsMap.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).floatValue();
    }


    @Nullable
    public Double getDouble(String key) {
        return getDouble(key, null);
    }

    @Nullable
    public Double getDouble(String key, @Nullable Double defaultValue) {
        String value = propsMap.get(key);

        if (CoreStrings.isEmpty(value)) {
            return defaultValue;
        }

        return CoreConversions.toNumber(value, defaultValue).doubleValue();
    }


    private CoreCacheBuilderSpec getCacheSpec(SquigglyConfigSource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        if (value == null) {
            value = "";
        }

        return CoreCacheBuilderSpec.parse(value);
    }

    private Boolean getBool(SquigglyConfigSource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        if (CoreStrings.isEmpty(value)) {
            return null;
        }

        return "true".equals(value);
    }

    private String getString(SquigglyConfigSource source, Map<String, String> props, Map<String, String> locations, String key) {
        String property = source.getProperty(key);
        String location = source.getLocation(key);
        props.put(key, property);
        locations.put(key, location);
        return property;
    }

    private int getInt(SquigglyConfigSource source, Map<String, String> props, Map<String, String> locations, String key) {
        String value = getString(source, props, locations, key);

        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            throw new RuntimeException("Unable to convert " + value + " to int for key " + key);
        }
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
     * @see com.github.bohnman.squiggly.core.view.PropertyView
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

    public SquigglyEnvironment getFunctionEnvironment() {
        return functionEnvironment;
    }

    /**
     * Get the {@link CoreCacheBuilderSpec} of the node cache in the squiggly parser.
     *
     * @return spec
     * @see com.github.bohnman.squiggly.core.parser.SquigglyParser
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
     * Determins whether to use the SquigglyContext in the node filter as last filter.
     *
     * @return true if use, false if not, or null if defer
     */
    public Boolean getAppendContextInNodeFilter() {
        return appendContextInNodeFilter;
    }

    /**
     * Gets all the config as a map.
     *
     * @return map
     */
    public SortedMap<String, String> asMap() {
        return propsMap;
    }

    /**
     * Gets a map of all the config keys and whose values are the location where that key was read from.
     *
     * @return source map
     */
    public SortedMap<String, String> asLocationMap() {
        return locationMap;
    }


    public static void main(String[] args) {
        SquigglyConfig config = new SquigglyConfig();
        System.out.println(config.asMap());
        System.out.println(config.asLocationMap());
    }
}
