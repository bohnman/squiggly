package com.github.bohnman.squiggly.filter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.model.Issue;
import com.github.bohnman.squiggly.model.IssueAction;
import com.github.bohnman.squiggly.model.Item;
import com.github.bohnman.squiggly.model.Outer;
import com.github.bohnman.squiggly.model.User;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import org.junit.Before;
import org.junit.Test;

import javax.annotation.Nullable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;

@SuppressWarnings("Duplicates")
public class SquigglyPropertyFilterTest {

    @Nullable
    private Issue issue;
    @Nullable
    private ObjectMapper objectMapper;
    private boolean init = false;
    @Nullable
    private Squiggly squiggly;

    @Before
    public void beforeEachTest() {
        issue = buildIssue();
        objectMapper = new ObjectMapper();
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
        raw = regexRemove(raw, ",\"entityType\":\"User\"");
        assertEquals(raw, stringify());
    }

    @Test
    public void testBaseView() {
        filter("base");
        String raw = regexRemove(stringifyRaw(), ",\"actions\":.*") + "}";
        raw = regexRemove(raw, ",\"entityType\":\"User\"");
        assertEquals(raw, stringify());
    }

    @Test
    public void testFullView() {
        filter("full");
        String raw = regexRemove(stringifyRaw(), ",\"userId\":\"[a-zA-Z0-9-_]+\"");
        raw = regexRemove(raw, ",\"user\":\\{.*?\\}");
        raw = regexRemove(raw, ",\"entityType\":\"User\"");

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
    public void testRegex() {
        filter("~iss[a-z]e.*~");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\",\"issueDetails\":\"" + issue.getIssueDetails() + "\"}", stringify());
    }

    @Test
    public void testRegexCaseInsensitive() {
        filter("~iss[a-z]esumm.*~i");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testRegexTraditional() {
        filter("/iss[a-z]e.*/");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\",\"issueDetails\":\"" + issue.getIssueDetails() + "\"}", stringify());
    }

    @Test
    public void testWildCardSingle() {
        filter("issueSummar?");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
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
    public void testDotPath() {
        filter("id,actions.user.firstName");
        assertEquals("{\"id\":\"ISSUE-1\",\"actions\":[{\"user\":{\"firstName\":\"Jorah\"}},{\"user\":{\"firstName\":\"Daario\"}}]}", stringify());
    }

    @Test
    public void testNegativeDotPath() {
        filter("id,-actions.user.firstName");
        assertEquals("{\"id\":\"ISSUE-1\",\"issueSummary\":\"Dragons Need Fed\",\"issueDetails\":\"I need my dragons fed pronto.\",\"reporter\":{\"firstName\":\"Daenerys\",\"lastName\":\"Targaryen\"},\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"},\"actions\":[{\"id\":null,\"type\":\"COMMENT\",\"text\":\"I'm going to let Daario get this one..\",\"user\":{\"lastName\":\"Mormont\"}},{\"id\":null,\"type\":\"CLOSE\",\"text\":\"All set.\",\"user\":{\"lastName\":\"Naharis\"}}]}", stringify());
    }

    @Test
    public void testNegativeDotPaths() {
        filter("-actions.user.firstName,-actions.user.lastName");
        assertEquals("{\"id\":\"ISSUE-1\",\"issueSummary\":\"Dragons Need Fed\",\"issueDetails\":\"I need my dragons fed pronto.\",\"reporter\":{\"firstName\":\"Daenerys\",\"lastName\":\"Targaryen\"},\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"},\"actions\":[{\"id\":null,\"type\":\"COMMENT\",\"text\":\"I'm going to let Daario get this one..\",\"user\":{}},{\"id\":null,\"type\":\"CLOSE\",\"text\":\"All set.\",\"user\":{}}]}", stringify());
    }

    @Test
    public void testNestedDotPath() {
        filter("id,actions.user[firstName],issueSummary");
        assertEquals("{\"id\":\"ISSUE-1\",\"issueSummary\":\"Dragons Need Fed\",\"actions\":[{\"user\":{\"firstName\":\"Jorah\"}},{\"user\":{\"firstName\":\"Daario\"}}]}", stringify());

        filter("id,actions.user[]");
        assertEquals("{\"id\":\"ISSUE-1\",\"actions\":[{\"user\":{}},{\"user\":{}}]}", stringify());
    }

    @Test
    public void testDeepNestedDotPath() {
        filter("id,items.items[items.id]");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"items\":[{\"id\":\"ITEM-4\"}]}]}]}", stringify(Item.testItem()));

        filter("id,items.items[items.items[id]]");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"items\":[{\"items\":[{\"id\":\"ITEM-5\"}]}]}]}]}", stringify(Item.testItem()));

        filter("id,items.items[-items.id]");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"id\":\"ITEM-3\",\"name\":\"Milkshake\",\"items\":[{\"name\":\"Hoverboard\",\"items\":[{\"id\":\"ITEM-5\",\"name\":\"Binoculars\",\"items\":[]}]}]}]}]}", stringify(Item.testItem()));
    }

    @Test
    public void testOtherView() {
        filter("other");
        String raw = regexRemove(stringifyRaw(), ",\"user\":\\{.*?\\}");
        raw = regexRemove(raw, ",\"properties\":\\{.*?\\}");
        raw = regexRemove(raw, ",\"entityType\":\"User\"");
        assertEquals(raw, stringify());
    }

    @Test
    public void testNestedEmpty() {
        filter("assignee[]");
        assertEquals("{\"assignee\":{}}", stringify());
    }

    @Test
    public void testAssignee() {
        filter("assignee");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());
    }

    @Test
    public void testNestedSingle() {
        filter("assignee[firstName]");
        assertEquals("{\"assignee\":{\"firstName\":\"" + issue.getAssignee().getFirstName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMultiple() {
        filter("actions[type,text]");
        assertEquals("{\"actions\":[{\"type\":\"" + issue.getActions().get(0).getType() + "\",\"text\":\"" + issue.getActions().get(0).getText() + "\"},{\"type\":\"" + issue.getActions().get(1).getType() + "\",\"text\":\"" + issue.getActions().get(1).getText() + "\"}]}", stringify());
    }

    @Test
    public void testMultipleNestedSingle() {
        filter("(reporter,assignee)[lastName]");
        assertEquals("{\"reporter\":{\"lastName\":\"" + issue.getReporter().getLastName() + "\"},\"assignee\":{\"lastName\":\"" + issue.getAssignee().getLastName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMap() {
        filter("properties[priority]");
        assertEquals("{\"properties\":{\"priority\":\"" + issue.getProperties().get("priority") + "\"}}", stringify());
    }

    @Test
    public void testDeepNested() {
        filter("actions[user[lastName]]");
        assertEquals("{\"actions\":[{\"user\":{\"lastName\":\"" + issue.getActions().get(0).getUser().getLastName() + "\"}},{\"user\":{\"lastName\":\"" + issue.getActions().get(1).getUser().getLastName() + "\"}}]}", stringify());
    }

    @Test
    public void testSameParent() {
        filter("assignee[firstName],assignee[lastName]");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());

        filter("assignee.firstName,assignee.lastName");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());

        filter("actions.user[firstName],actions.user[lastName]");
        assertEquals("{\"actions\":[{\"user\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}},{\"user\":{\"firstName\":\"Daario\",\"lastName\":\"Naharis\"}}]}", stringify());
    }

    @Test
    public void testFilterExcludesBaseFieldsInView() {
        String fieldName = "filterImplicitlyIncludeBaseFieldsInView";
        filter("view1");

        try {
            setFieldValue(squiggly.getConfig(), fieldName, false);
            assertEquals("{\"properties\":" + stringifyRaw(issue.getProperties()) + "}", stringify());
        } finally {
            setFieldValue(squiggly.getConfig(), fieldName, true);
        }
    }

    @Test
    public void testPropagateViewToNestedFilters() {
        String fieldName = "filterPropagateViewToNestedFilters";
        filter("full");

        try {
            setFieldValue(squiggly.getConfig(), fieldName, true);
            assertEquals(stringifyRaw(), stringify());
        } finally {
            setFieldValue(squiggly.getConfig(), fieldName, false);
        }
    }

    @Test
    public void testPropertyAddNonAnnotatedFieldsToBaseView() {
        String fieldName = "propertyAddNonAnnotatedFieldsToBaseView";
        filter("base");

        try {
            setFieldValue(squiggly.getConfig(), fieldName, false);
            assertEquals("{}", stringify());
        } finally {
            setFieldValue(squiggly.getConfig(), fieldName, true);
        }
    }

    @Test
    public void testFilterSpecificty() {
        filter("**,reporter[lastName,entityType]");
        String raw = stringifyRaw();
        assertEquals(raw.replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());

        filter("**,repo*[lastName,entityType],repo*[firstName,entityType]");
        assertEquals(raw, stringify());

        filter("**,reporter[lastName,entityType],repo*[firstName,entityType]");
        assertEquals(raw.replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());

        filter("**,repo*[firstName,entityType],rep*[lastName,entityType]");
        assertEquals(raw.replace(",\"lastName\":\"" + issue.getReporter().getLastName() + "\"", ""), stringify());

        filter("**,reporter[firstName,entityType],reporter[lastName,entityType]");
        assertEquals(raw, stringify());
    }

    @Test
    public void testFilterExclusion() {
        filter("**,reporter[-firstName]");
        assertEquals("{\"id\":\"ISSUE-1\",\"issueSummary\":\"Dragons Need Fed\",\"issueDetails\":\"I need my dragons fed pronto.\",\"reporter\":{\"lastName\":\"Targaryen\"},\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\",\"entityType\":\"User\"},\"actions\":[{\"id\":null,\"type\":\"COMMENT\",\"text\":\"I'm going to let Daario get this one..\",\"user\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\",\"entityType\":\"User\"}},{\"id\":null,\"type\":\"CLOSE\",\"text\":\"All set.\",\"user\":{\"firstName\":\"Daario\",\"lastName\":\"Naharis\",\"entityType\":\"User\"}}],\"properties\":{\"priority\":\"1\",\"email\":\"motherofdragons@got.com\"}}", stringify());
    }

    @Test
    public void testFunction() {
        filter("id,actions.limit(2){firstName}");
        System.out.println(stringify());
    }

    @Test
    public void testJsonUnwrapped() {
        filter("innerText");
        assertEquals("{\"innerText\":\"innerValue\"}", stringify(new Outer("outerValue", "innerValue")));
    }

    private void setFieldValue(Object object, String fieldName, boolean value) {
        Field field = getField(object, fieldName);
        try {
            field.setBoolean(object, value);
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

    private Field getField(Object object, String fieldName) {
        try {
            Field field = object.getClass().getDeclaredField(fieldName);
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

    @SuppressWarnings("UnusedReturnValue")
    private String filter(String filter) {
        this.squiggly = Squiggly.builder(filter).build();
        this.squiggly.apply(objectMapper);
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
