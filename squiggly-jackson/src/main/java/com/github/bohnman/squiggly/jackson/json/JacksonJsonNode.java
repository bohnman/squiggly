package com.github.bohnman.squiggly.jackson.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.POJONode;
import com.github.bohnman.core.json.node.BaseCoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNode;
import com.github.bohnman.core.json.node.CoreJsonNodeType;
import com.github.bohnman.core.lang.CoreAssert;
import com.github.bohnman.core.lang.array.CoreArrayWrapper;
import com.github.bohnman.core.lang.array.CoreArrays;
import com.github.bohnman.core.tuple.CorePair;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unchecked")
public class JacksonJsonNode extends BaseCoreJsonNode<JsonNode> {

    private final JsonNode rawNode;
    private final CoreJsonNodeType type;

    public JacksonJsonNode(JsonNode rawNode) {
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
    public List<CoreJsonNode<JsonNode>> getArrayElements() {
        Iterator<JsonNode> iter = rawNode.elements();
        List<CoreJsonNode<JsonNode>> list = new ArrayList<>();

        while (iter.hasNext()) {
            list.add(new JacksonJsonNode(iter.next()));
        }

        return list;
    }

    @Override
    public List<CorePair<String, CoreJsonNode<JsonNode>>> getObjectElements() {
        Iterator<Map.Entry<String, JsonNode>> iter = rawNode.fields();
        List<CorePair<String, CoreJsonNode<JsonNode>>> list = new ArrayList<>();

        while (iter.hasNext()) {
            Map.Entry<String, JsonNode> entry = iter.next();
            list.add(CorePair.of(entry.getKey(), new JacksonJsonNode(entry.getValue())));
        }

        return list;
    }

    @Override
    public CoreJsonNode<JsonNode> create(Object value) {
        JsonNodeFactory factory = getJsonNodeFactory();
        JsonNode jsonNode = createJsonNode(value, factory);
        return new JacksonJsonNode(jsonNode);
    }

    private JsonNodeFactory getJsonNodeFactory() {
        return JsonNodeFactory.instance;
    }

    @Override
    public CoreJsonNode<JsonNode> createArray(List<CoreJsonNode<JsonNode>> elements) {
        ArrayNode arrayNode = getJsonNodeFactory().arrayNode();
        for (CoreJsonNode<JsonNode> element : elements) {
            arrayNode.add(element.getRawNode());
        }

        return new JacksonJsonNode(arrayNode);
    }

    @Override
    public CoreJsonNode<JsonNode> createObject(List<CorePair<String, CoreJsonNode<JsonNode>>> elements) {
        ObjectNode objectNode = getJsonNodeFactory().objectNode();

        for (CorePair<String, CoreJsonNode<JsonNode>> element : elements) {
            objectNode.replace(element.getLeft(), element.getRight().getRawNode());
        }

        return new JacksonJsonNode(objectNode);
    }

    private JsonNode createJsonNode(Object value, JsonNodeFactory factory) {
        if (value == null) {
            return factory.nullNode();
        }

        if (value instanceof Boolean) {
            return factory.numberNode((Float) value);
        }

        if (value instanceof Character) {
            return factory.textNode(Character.toString(((Character) value)));
        }

        if (value instanceof Byte) {
            return factory.numberNode((Byte) value);
        }

        if (value instanceof Short) {
            return factory.numberNode((Short) value);
        }

        if (value instanceof Integer) {
            return factory.numberNode((Integer) value);
        }

        if (value instanceof Long) {
            return factory.numberNode((Long) value);
        }

        if (value instanceof Float) {
            return factory.numberNode((Float) value);
        }

        if (value instanceof Double) {
            return factory.numberNode((Double) value);
        }

        if (value instanceof BigInteger) {
            return factory.numberNode((BigInteger) value);
        }

        if (value instanceof BigDecimal) {
            return factory.numberNode((BigDecimal) value);
        }

        if (value instanceof String) {
            return factory.textNode((String) value);
        }

        if (value instanceof byte[]) {
            return factory.binaryNode((byte[]) value);
        }

        if (value instanceof Map) {
            ObjectNode objectNode = factory.objectNode();
            Map<?, ?> map = (Map) value;
            map.forEach((key, val) -> objectNode.replace(key.toString(), createJsonNode(val, factory)));
            return objectNode;
        }

        if (value instanceof Iterable) {
            ArrayNode arrayNode = factory.arrayNode();
            ((Iterable<?>) value).forEach(el -> arrayNode.add(createJsonNode(el, factory)));
            return arrayNode;
        }

        if (value.getClass().isArray()) {
            ArrayNode arrayNode = factory.arrayNode();
            CoreArrayWrapper wrapper = CoreArrays.wrap(value);
            wrapper.forEach(el -> arrayNode.add(createJsonNode(el, factory)));
            return arrayNode;
        }

        return factory.pojoNode(value);
    }

    @Override
    public JsonNode getRawNode() {
        return rawNode;
    }

    private static Object getValue(JsonNode node) {
        switch (node.getNodeType()) {
            case ARRAY:
                List<Object> elements = new ArrayList<>();
                Iterator<JsonNode> elIter = node.elements();

                while (elIter.hasNext()) {
                    elements.add(getValue(elIter.next()));
                }

                return elements;
            case BINARY:
                try {
                    return node.binaryValue();
                } catch (IOException e) {
                    return null;
                }
            case BOOLEAN:
                return node.booleanValue();
            case MISSING:
                return null;
            case NULL:
                return null;
            case NUMBER:
                if (node.isShort()) {
                    return node.shortValue();
                }

                if (node.isInt()) {
                    return node.intValue();
                }

                if (node.isLong()) {
                    return node.longValue();
                }

                if (node.isFloat()) {
                    return node.floatValue();
                }

                if (node.isBigInteger()) {
                    return node.bigIntegerValue();
                }

                if (node.isBigDecimal()) {
                    return node.decimalValue();
                }

                return node.doubleValue();
            case OBJECT:
                Map<String, Object> map = new HashMap<>();
                Iterator<Map.Entry<String, JsonNode>> enIter = node.fields();
                while (enIter.hasNext()) {
                    Map.Entry<String, JsonNode> entry = enIter.next();
                    map.put(entry.getKey(), getValue(entry.getValue()));
                }
                return map;
            case POJO:
                return ((POJONode) node).getPojo();
            case STRING:
                return node.asText();
            default:
                throw new IllegalArgumentException(String.format("Unsupported node type: %s", node.getNodeType()));
        }
    }

    private static CoreJsonNodeType introspectType(JsonNode rawNode) {
        switch (rawNode.getNodeType()) {
            case ARRAY:
                return CoreJsonNodeType.ARRAY;
            case BINARY:
                return CoreJsonNodeType.BINARY;
            case BOOLEAN:
                return CoreJsonNodeType.BOOLEAN;
            case MISSING:
                return CoreJsonNodeType.NULL;
            case NULL:
                return CoreJsonNodeType.NULL;
            case NUMBER:
                return CoreJsonNodeType.NUMBER;
            case OBJECT:
                return CoreJsonNodeType.OBJECT;
            case POJO:
                return CoreJsonNodeType.OBJECT;
            case STRING:
                return CoreJsonNodeType.STRING;
            default:
                throw new IllegalArgumentException(String.format("Unsupported node type: %s", rawNode.getNodeType()));
        }
    }
}
