package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.github.bohnman.squiggly.config.SquigglyConfig;
import com.github.bohnman.squiggly.context.provider.SimpleSquigglyContextProvider;
import com.github.bohnman.squiggly.model.Issue;
import com.github.bohnman.squiggly.model.IssueAction;
import com.github.bohnman.squiggly.model.User;
import com.github.bohnman.squiggly.parser.SquigglyParser;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;

@SuppressWarnings("Duplicates")
public class SquigglyPropertyFilterTests {

    private Issue issue;
    private ObjectMapper objectMapper;
    private SimpleFilterProvider filterProvider;
    private boolean init = false;

    @Before
    public void beforeEachTest() {
        if (!init) {
            issue = buildIssue();
            objectMapper = new ObjectMapper();
            filterProvider = new SimpleFilterProvider();
            objectMapper.setFilterProvider(filterProvider);
            objectMapper.addMixIn(Object.class, SquigglyPropertyFilterMixin.class);
            init = true;
        }

        filterProvider.removeFilter(SquigglyPropertyFilter.FILTER_ID);
    }

    private Issue buildIssue() {
        Map<String, Object> properties = new HashMap<>();
        properties.put("email", "motherofdragons@got.com");
        properties.put("priority", "1");

        Issue issue = new Issue();
        issue.setId("ISSUE-1");
        issue.setIssueSummary("Dragons Need Fed");
        issue.setIssueDetails("I need my dragons fed pronto.");
        User assignee = new User("Jorah", "Mormont");
        issue.setAssignee(assignee);
        issue.setReporter(new User("Daenerys", "Targaryen"));
        issue.setActions(Arrays.asList(
                new IssueAction("COMMENT", "I'm going to let Daario get this one..", assignee),
                new IssueAction("CLOSE", "All set.", new User("Daario", "Naharis"))
        ));
        issue.setProperties(properties);
        return issue;
    }


    @Test
    public void testAnyDeep() {
        filter("**");
        assertEquals(stringifyRaw(), stringify());
    }

    @Test
    public void testAnyShallow() {
        filter("*");
        String raw = regexRemove(stringifyRaw(), ",\"userId\":\"[a-zA-Z0-9-_]+\"");
        raw = regexRemove(raw, ",\"user\":\\{.*?\\}");
        assertEquals(raw, stringify());
    }

    @Test
    public void testBaseView() {
        filter("base");
        assertEquals(regexRemove(stringifyRaw(), ",\"actions\":.*") + "}", stringify());
    }

    @Test
    public void testFullView() {
        filter("full");
        String raw = regexRemove(stringifyRaw(), ",\"userId\":\"[a-zA-Z0-9-_]+\"");
        raw = regexRemove(raw, ",\"user\":\\{.*?\\}");

        assertEquals(raw, stringify());
    }

    @Test
    public void testEmpty() {
        filter("");
        assertEquals("{}", stringify());
    }

    @Test
    public void testSingleField() {
        filter("id");
        assertEquals("{\"id\":\"" + issue.getId() + "\"}", stringify());
    }

    @Test
    public void testMultipleFields() {
        filter("id,issueSummary");
        assertEquals("{\"id\":\"" + issue.getId() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testWildCardStart() {
        filter("issue*");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\",\"issueDetails\":\"" + issue.getIssueDetails() + "\"}", stringify());
    }

    @Test
    public void testWildCardEnd() {
        filter("*d");
        assertEquals("{\"id\":\"" + issue.getId() + "\"}", stringify());
    }

    @Test
    public void testWildCardMiddle() {
        filter("*ue*");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\",\"issueDetails\":\"" + issue.getIssueDetails() + "\"}", stringify());
    }

    @Test
    public void testOtherView() {
        filter("other");
        String raw = regexRemove(stringifyRaw(), ",\"user\":\\{.*?\\}");
        raw = regexRemove(raw, ",\"properties\":\\{.*?\\}");
        assertEquals(raw, stringify());
    }


    @Test
    public void testNestedSingle() {
        filter("assignee{firstName}");
        assertEquals("{\"assignee\":{\"firstName\":\"" + issue.getAssignee().getFirstName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMultiple() {
        filter("actions{type,text}");
        assertEquals("{\"actions\":[{\"type\":\"" + issue.getActions().get(0).getType() + "\",\"text\":\"" + issue.getActions().get(0).getText() + "\"},{\"type\":\"" + issue.getActions().get(1).getType() + "\",\"text\":\"" + issue.getActions().get(1).getText() + "\"}]}", stringify());
    }

    @Test
    public void testMultipleNestedSingle() {
        filter("reporter|assignee{lastName}");
        assertEquals("{\"reporter\":{\"lastName\":\"" + issue.getReporter().getLastName() + "\"},\"assignee\":{\"lastName\":\"" + issue.getAssignee().getLastName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMap() {
        filter("properties{priority}");
        assertEquals("{\"properties\":{\"priority\":\"" + issue.getProperties().get("priority") + "\"}}", stringify());
    }

    @Test
    public void testDeepNested() {
        filter("actions{user{lastName}}");
        assertEquals("{\"actions\":[{\"user\":{\"lastName\":\"" + issue.getActions().get(0).getUser().getLastName() + "\"}},{\"user\":{\"lastName\":\"" + issue.getActions().get(1).getUser().getLastName() + "\"}}]}", stringify());
    }

    @Test
    public void testFilterExcludesBaseFields() {
        String fieldName = "filterImplicitlyIncludeBaseFields";

        try {
            setFieldValue(SquigglyConfig.class, fieldName, false);
            filter("view1");
            assertEquals("{\"properties\":" + stringifyRaw(issue.getProperties()) + "}", stringify());
        } finally {
            setFieldValue(SquigglyConfig.class, fieldName, true);
        }
    }

    @Test
    public void testPropagateViewToNestedFilters() {
        String fieldName = "filterPropagateViewToNestedFilters";

        try {
            setFieldValue(SquigglyConfig.class, fieldName, true);
            filter("full");
            assertEquals(stringifyRaw(), stringify());
        } finally {
            setFieldValue(SquigglyConfig.class, fieldName, false);
        }
    }

    @Test
    public void testPropertyAddNonAnnotatedFieldsToBaseView() {
        String fieldName = "propertyAddNonAnnotatedFieldsToBaseView";

        try {
            setFieldValue(SquigglyConfig.class, fieldName, false);
            filter("base");
            assertEquals("{}", stringify());
        } finally {
            setFieldValue(SquigglyConfig.class, fieldName, true);
        }
    }

    @Test
    public void testFilterSpecificty() {
        filter("**,reporter{lastName}");
        assertEquals(stringifyRaw().replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());
    }

    @Test
    public void testFilterExclusion() {
        filter("**,reporter{**,-firstName}");
        assertEquals(stringifyRaw().replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());
    }

    private void setFieldValue(Class<?> ownerClass, String fieldName, boolean value) {
        Field field = getField(ownerClass, fieldName);
        try {
            field.setBoolean(null, value);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    private void removeFinalModifier(Field field) {
        try {
            Field modifiersField = Field.class.getDeclaredField("modifiers");
            modifiersField.setAccessible(true);
            modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private Field getField(Class<?> ownerClass, String fieldName) {
        try {
            Field field = ownerClass.getDeclaredField(fieldName);
            field.setAccessible(true);
            removeFinalModifier(field);
            return field;
        } catch (NoSuchFieldException e) {
            throw new RuntimeException(e);
        }
    }

    private String regexRemove(String input, String regex) {
        Matcher matcher = regex(input, regex);
        StringBuffer sb = new StringBuffer();

        while (matcher.find()) {
            matcher.appendReplacement(sb, "");
        }
        matcher.appendTail(sb);
        return sb.toString();
    }

    private Matcher regex(String input, String regex) {
        Pattern pattern = Pattern.compile(regex);
        return pattern.matcher(input);
    }

    private String filter(String filter) {
        SquigglyParser parser = new SquigglyParser();
        SimpleSquigglyContextProvider provider = new SimpleSquigglyContextProvider(parser, filter);
        filterProvider.addFilter(SquigglyPropertyFilter.FILTER_ID, new SquigglyPropertyFilter(provider));
        return filter;
    }

    private String stringify() {
        return stringify(issue);
    }

    private String stringify(Object object) {
        return SquigglyUtils.stringify(objectMapper, object);
    }


    private String stringifyRaw() {
        return stringifyRaw(issue);
    }

    private String stringifyRaw(Object object) {
        return SquigglyUtils.stringify(new ObjectMapper(), object);
    }

}
