package com.github.bohnman.squiggly.gson.json;

import com.github.bohnman.core.bean.CoreBeans;
import com.github.bohnman.core.collect.CoreArrayWrapper;
import com.github.bohnman.core.collect.CoreArrays;
import com.github.bohnman.core.collect.CoreStreams;
import com.github.bohnman.core.json.node.BaseCoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNodeType;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.CoreMethods;
import com.github.bohnman.core.tuple.CorePair;
import com.google.gson.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;

/**
 * Custom CoreJsonNode implementation that works with Gson nodes.
 */
@SuppressWarnings("unchecked")
public class GsonJsonNode extends BaseCoreJsonNode<JsonElement> {

    private final JsonElement rawNode;
    private final CoreJsonNodeType type;

    /**
     * Construct the CoreNode wrapper with the raw Gson node.
     *
     * @param rawNode raw Gson node
     */
    public GsonJsonNode(JsonElement rawNode) {
        this.rawNode = CoreAssert.notNull(rawNode);
        this.type = introspectType(rawNode);
    }

    @Override
    public CoreJsonNodeType getType() {
        return type;
    }

    @Override
    public Object getValue() {
        return getValue(rawNode);
    }

    @Override
    public List<CoreJsonNode<JsonElement>> getArrayElements() {
        if (type != CoreJsonNodeType.ARRAY) {
            return Collections.emptyList();
        }

        JsonArray arrayNode = (JsonArray) rawNode;

        return CoreStreams.of(arrayNode)
                .map(GsonJsonNode::new)
                .collect(Collectors.toList());
    }

    @Override
    public List<CorePair<String, CoreJsonNode<JsonElement>>> getObjectElements() {
        if (type != CoreJsonNodeType.OBJECT) {
            return Collections.emptyList();
        }

        JsonObject objectNode = (JsonObject) rawNode;
        return objectNode.entrySet().stream()
                .map(entry -> CorePair.of(entry.getKey(), (CoreJsonNode<JsonElement>) new GsonJsonNode(entry.getValue())))
                .collect(Collectors.toList());
    }

    @Override
    public CoreJsonNode<JsonElement> create(Object value) {
        JsonElement JsonElement = createJsonElement(value);
        return new GsonJsonNode(JsonElement);
    }


    @Override
    public CoreJsonNode<JsonElement> createArray(List<CoreJsonNode<JsonElement>> elements) {
        JsonArray arrayNode = new JsonArray(elements.size());
        elements.forEach(element -> arrayNode.add(element.getRawNode()));
        return new GsonJsonNode(arrayNode);
    }

    @Override
    public CoreJsonNode<JsonElement> createObject(List<CorePair<String, CoreJsonNode<JsonElement>>> elements) {
        JsonObject objectNode = new JsonObject();
        elements.forEach(element -> objectNode.add(element.getLeft(), element.getRight().getRawNode()));
        return new GsonJsonNode(objectNode);
    }

    private JsonElement createJsonElement(Object value) {
        if (value == null) {
            return JsonNull.INSTANCE;
        }

        if (value instanceof Boolean) {
            return new JsonPrimitive((Boolean) value);
        }

        if (value instanceof Character) {
            return new JsonPrimitive(Character.toString(((Character) value)));
        }

        if (value instanceof Byte) {
            return new JsonPrimitive((Byte) value);
        }

        if (value instanceof Short) {
            return new JsonPrimitive((Short) value);
        }

        if (value instanceof Integer) {
            return new JsonPrimitive((Integer) value);
        }

        if (value instanceof Long) {
            return new JsonPrimitive((Long) value);
        }

        if (value instanceof Float) {
            return new JsonPrimitive((Float) value);
        }

        if (value instanceof Double) {
            return new JsonPrimitive((Double) value);
        }

        if (value instanceof BigInteger) {
            return new JsonPrimitive((BigInteger) value);
        }

        if (value instanceof BigDecimal) {
            return new JsonPrimitive((BigDecimal) value);
        }

        if (value instanceof String) {
            return new JsonPrimitive((String) value);
        }

        if (value instanceof Map) {
            JsonObject objectNode = new JsonObject();
            Map<?, ?> map = (Map) value;
            map.forEach((key, val) -> objectNode.add(key.toString(), createJsonElement(val)));
            return objectNode;
        }

        if (value instanceof Iterable) {
            JsonArray arrayNode = new JsonArray();
            ((Iterable<?>) value).forEach(el -> arrayNode.add(createJsonElement(el)));
            return arrayNode;
        }

        if (value.getClass().isArray()) {
            JsonArray arrayNode = new JsonArray();
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            wrapper.forEach(el -> arrayNode.add(createJsonElement(el)));
            return arrayNode;
        }

        JsonObject objectNode = new JsonObject();

        CoreBeans.getReadablePropertyDescriptors(value.getClass())
                .forEach(pd -> {
                    Object propertyValue = CoreMethods.invoke(pd.getReadMethod(), value);
                    JsonElement element = createJsonElement(propertyValue);
                    objectNode.add(pd.getName(), element);
                });
        return objectNode;
    }

    @Override
    public JsonElement getRawNode() {
        return rawNode;
    }

    private static Object getValue(JsonElement rawNode) {
        if (rawNode.isJsonArray()) {
            return CoreStreams.of((JsonArray) rawNode)
                    .map(GsonJsonNode::getValue)
                    .collect(Collectors.toList());
        }

        if (rawNode.isJsonObject()) {
            return ((JsonObject) rawNode).entrySet().stream()
                    .collect(toMap(Map.Entry::getKey, e -> getValue(e.getValue())));
        }

        if (rawNode.isJsonNull()) {
            return null;
        }

        if (rawNode.isJsonPrimitive()) {
            JsonPrimitive primitive = (JsonPrimitive) rawNode;

            if (primitive.isBoolean()) {
                return rawNode.getAsBoolean();
            }

            if (primitive.isString()) {
                return rawNode.getAsString();
            }

            if (primitive.isNumber()) {
                return rawNode.getAsDouble();
            }
        }

        throw new IllegalArgumentException(String.format("Unsupported node type: %s", rawNode));
    }

    private static CoreJsonNodeType introspectType(JsonElement rawNode) {
        if (rawNode.isJsonArray()) {
            return CoreJsonNodeType.ARRAY;
        }

        if (rawNode.isJsonObject()) {
            return CoreJsonNodeType.OBJECT;
        }

        if (rawNode.isJsonNull()) {
            return CoreJsonNodeType.NULL;
        }

        if (rawNode.isJsonPrimitive()) {
            JsonPrimitive primitive = (JsonPrimitive) rawNode;

            if (primitive.isBoolean()) {
                return CoreJsonNodeType.BOOLEAN;
            }

            if (primitive.isString()) {
                return CoreJsonNodeType.STRING;
            }

            if (primitive.isNumber()) {
                return CoreJsonNodeType.NUMBER;
            }
        }

        throw new IllegalArgumentException(String.format("Unsupported node type: %s", rawNode));
    }
}
