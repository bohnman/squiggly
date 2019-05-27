package com.github.bohnman.squiggly.jackson.filter;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.github.bohnman.core.json.jackson.CoreObjectMappers;
import com.github.bohnman.squiggly.jackson.SquigglyJackson;
import com.github.bohnman.squiggly.jackson.model.*;
import org.junit.Before;
import org.junit.Test;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.Assert.assertEquals;

@SuppressWarnings("Duplicates")
public class SquigglyJacksonPropertyFilterTest {
    public static final String BASE_PATH = "com/github/bohnman/squiggly/jackson/filter/SquigglyPropertyFilterTest";

    private Issue issue;


    @Nullable
    private ObjectMapper objectMapper;
    private ObjectMapper rawObjectMapper = new ObjectMapper();

    @Nullable
    private SquigglyJackson squiggly;

    public SquigglyJacksonPropertyFilterTest() {
        rawObjectMapper.configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true);
        rawObjectMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
    }

    @Before
    public void beforeEachTest() {
        issue = buildIssue();
        objectMapper = new ObjectMapper();
        objectMapper.configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true);
        objectMapper.configure(MapperFeature.SORT_PROPERTIES_ALPHABETICALLY, true);
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
        raw = regexRemove(raw, "\"entityType\":\"User\",");
        assertEquals(raw, stringify());
    }

    @Test
    public void testBaseView() {
        filter("base");
        String raw = stringifyRaw();
        raw = regexRemove(raw, "\"actions\":\\[.*?\\],");
        raw = regexRemove(raw, "\"entityType\":\"User\",");
        raw = regexRemove(raw, "\"properties\":\\{.*?\\},");
        assertEquals(raw, stringify());
    }


    @Test
    public void testEmpty() {
        filter("");
        assertEquals("{}", stringify());
    }

    @Test
    public void testFullView() {
        filter("full");
        String raw = regexRemove(stringifyRaw(), ",\"userId\":\"[a-zA-Z0-9-_]+\"");
        raw = regexRemove(raw, ",\"user\":\\{.*?\\}");
        raw = regexRemove(raw, "\"entityType\":\"User\",");

        assertEquals(raw, stringify());
    }

    @Test
    public void testSingleField() {
        filter("id");
        assertEquals("{\"id\":\"" + issue.getId() + "\"}", stringify());

        filter("assignee");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());
    }

    @Test
    public void testMultipleFields() {
        filter("id,issueSummary");
        assertEquals("{\"id\":\"" + issue.getId() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testRegex() {
        filter("~iss[a-z]e.*~");
        assertEquals("{\"issueDetails\":\"" + issue.getIssueDetails() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testRegexCaseInsensitive() {
        filter("~iss[a-z]esumm.*~i");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testRegexTraditional() {
        filter("/iss[a-z]e.*/");
        assertEquals("{\"issueDetails\":\"" + issue.getIssueDetails() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testWildCardEnd() {
        filter("*d");
        assertEquals("{\"id\":\"" + issue.getId() + "\"}", stringify());
    }

    @Test
    public void testWildCardMiddle() {
        filter("*ue*");
        assertEquals("{\"issueDetails\":\"" + issue.getIssueDetails() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testWildCardSingle() {
        filter("issueSummar?");
        assertEquals("{\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }

    @Test
    public void testWildCardStart() {
        filter("issue*");
        assertEquals("{\"issueDetails\":\"" + issue.getIssueDetails() + "\",\"issueSummary\":\"" + issue.getIssueSummary() + "\"}", stringify());
    }


    @Test
    public void testNegativeDotPath() {
        filter("id,-actions.user.firstName");
        assertEquals("{\"actions\":[{\"id\":null,\"text\":\"I'm going to let Daario get this one..\",\"type\":\"COMMENT\",\"user\":{\"lastName\":\"Mormont\"}},{\"id\":null,\"text\":\"All set.\",\"type\":\"CLOSE\",\"user\":{\"lastName\":\"Naharis\"}}],\"id\":\"ISSUE-1\"}", stringify());
    }

    @Test
    public void testNegativeDotPaths() {
        filter("-actions.user.firstName,-actions.user.lastName");
        assertEquals("{\"actions\":[{\"id\":null,\"text\":\"I'm going to let Daario get this one..\",\"type\":\"COMMENT\",\"user\":{}},{\"id\":null,\"text\":\"All set.\",\"type\":\"CLOSE\",\"user\":{}}],\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"},\"id\":\"ISSUE-1\",\"issueDetails\":\"I need my dragons fed pronto.\",\"issueSummary\":\"Dragons Need Fed\",\"reporter\":{\"firstName\":\"Daenerys\",\"lastName\":\"Targaryen\"}}", stringify());
    }

    @Test
    public void testNestedDotPath() {
        filter("id,actions.user(firstName),issueSummary");
        assertEquals("{\"actions\":[{\"user\":{\"firstName\":\"Jorah\"}},{\"user\":{\"firstName\":\"Daario\"}}],\"id\":\"ISSUE-1\",\"issueSummary\":\"Dragons Need Fed\"}", stringify());

        filter("id,actions.user()");
        assertEquals("{\"actions\":[{\"user\":{}},{\"user\":{}}],\"id\":\"ISSUE-1\"}", stringify());
    }

    @Test
    public void testDeepNestedDotPath() {
        filter("id,items.items(items.id)");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"items\":[{\"id\":\"ITEM-4\"}]}]}]}", stringify(Item.testItem()));

        filter("id,items.items(items.items(id))");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"items\":[{\"items\":[{\"id\":\"ITEM-5\"}]}]}]}]}", stringify(Item.testItem()));

        filter("id,items.items(-items.id)");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"id\":\"ITEM-3\",\"items\":[{\"items\":[{\"id\":\"ITEM-5\",\"items\":[],\"name\":\"Binoculars\"}],\"name\":\"Hoverboard\"}],\"name\":\"Milkshake\"}]}]}", stringify(Item.testItem()));

        filter("id,items.items(items(-id,-name),id)");
        assertEquals("{\"id\":\"ITEM-1\",\"items\":[{\"items\":[{\"id\":\"ITEM-3\",\"items\":[{\"items\":[{\"id\":\"ITEM-5\",\"items\":[],\"name\":\"Binoculars\"}]}]}]}]}", stringify(Item.testItem()));

        fileTest("company-list.json", "deep-nested-01-filter.txt", "deep-nested-01-expected.json");
        fileTest("task-list.json", "deep-nested-02-filter.txt", "deep-nested-02-expected.json");
        fileTest("task-list.json", "deep-nested-03-filter.txt", "deep-nested-03-expected.json");
    }

    @Test
    public void testOtherView() {
        filter("other");
        String raw = regexRemove(stringifyRaw(), ",\"user\":\\{.*?\\}");
        raw = regexRemove(raw, ",\"properties\":\\{.*?\\}");
        raw = regexRemove(raw, "\"entityType\":\"User\",");
        assertEquals(raw, stringify());
    }

    @Test
    public void testNestedEmpty() {
        filter("assignee()");
        assertEquals("{\"assignee\":{}}", stringify());
    }

    @Test
    public void testNestedSingle() {
        filter("assignee(firstName)");
        assertEquals("{\"assignee\":{\"firstName\":\"" + issue.getAssignee().getFirstName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMultiple() {
        filter("actions(type,text)");

        assertEquals("{\"actions\":[{\"text\":\"" + issue.getActions().get(0).getText() + "\",\"type\":\"" + issue.getActions().get(0).getType() + "\"},{\"text\":\"" + issue.getActions().get(1).getText() + "\",\"type\":\"" + issue.getActions().get(1).getType() + "\"}]}", stringify());
    }

    @Test
    public void testMultipleNestedSingle() {
        filter("(reporter,assignee)(lastName)");
        assertEquals("{\"assignee\":{\"lastName\":\"" + issue.getAssignee().getLastName() + "\"},\"reporter\":{\"lastName\":\"" + issue.getReporter().getLastName() + "\"}}", stringify());
    }

    @Test
    public void testNestedMap() {
        filter("properties(priority)");
        assertEquals("{\"properties\":{\"priority\":\"" + issue.getProperties().get("priority") + "\"}}", stringify());
    }

    @Test
    public void testDeepNested() {
        filter("actions(user(lastName))");
        assertEquals("{\"actions\":[{\"user\":{\"lastName\":\"" + issue.getActions().get(0).getUser().getLastName() + "\"}},{\"user\":{\"lastName\":\"" + issue.getActions().get(1).getUser().getLastName() + "\"}}]}", stringify());
    }

    @Test
    public void testSameParent() {
        filter("assignee(firstName),assignee(lastName)");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());

        filter("assignee.firstName,assignee.lastName");
        assertEquals("{\"assignee\":{\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}}", stringify());

        filter("actions.user(firstName),actions.user(lastName)");
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
    public void testFilterSpecificity() {
        filter("**,reporter(lastName,entityType)");
        String raw = stringifyRaw();
        assertEquals(raw.replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());

        filter("**,repo*(lastName,entityType),repo*(firstName,entityType)");
        assertEquals(raw, stringify());

        filter("**,reporter(lastName,entityType),repo*(firstName,entityType)");
        assertEquals(raw.replace("\"firstName\":\"" + issue.getReporter().getFirstName() + "\",", ""), stringify());

        filter("**,repo*(firstName,entityType),rep*(lastName,entityType)");
        assertEquals(raw.replace(",\"lastName\":\"" + issue.getReporter().getLastName() + "\"", ""), stringify());

        filter("**,reporter(firstName,entityType),reporter(lastName,entityType)");
        assertEquals(raw, stringify());
    }

    @Test
    public void testFilterExclusion() {
        filter("**,reporter(-firstName)");
        assertEquals("{\"actions\":[{\"id\":null,\"text\":\"I'm going to let Daario get this one..\",\"type\":\"COMMENT\",\"user\":{\"entityType\":\"User\",\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"}},{\"id\":null,\"text\":\"All set.\",\"type\":\"CLOSE\",\"user\":{\"entityType\":\"User\",\"firstName\":\"Daario\",\"lastName\":\"Naharis\"}}],\"assignee\":{\"entityType\":\"User\",\"firstName\":\"Jorah\",\"lastName\":\"Mormont\"},\"id\":\"ISSUE-1\",\"issueDetails\":\"I need my dragons fed pronto.\",\"issueSummary\":\"Dragons Need Fed\",\"properties\":{\"email\":\"motherofdragons@got.com\",\"priority\":\"1\"},\"reporter\":{\"lastName\":\"Targaryen\"}}", stringify());
    }


    // TODO: finish
    @Test
    public void testFunction() {
        filter("id,actions@limit(2)(firstName)");
        System.out.println(stringify());
    }

    @Test
    public void testJsonUnwrapped() {
        filter("innerText");
        assertEquals("{\"innerText\":\"innerValue\"}", stringify(new Outer("outerValue", "innerValue")));
    }

     @Test
    public void testPropertyWithDash() {
        filter("full-name");
        assertEquals("{\"full-name\":\"Fred Flintstone\"}", stringify(new DashObject("ID-1", "Fred Flintstone")));
    }

    // TODO: finish
    @Test
    public void testDeep() {
//        filter("**(other)");
//        assertEquals("{s}", stringify());

        filter("**(*,firstName='Tony')");
        assertEquals(regexReplace(stringifyRaw(), "(\"firstName\":)\".*?\"", "$1\"Tony\""), stringify());
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

    private String regexReplace(String input, String regex, String replacement) {
        Matcher matcher = regex(input, regex);
        return matcher.replaceAll(replacement);
    }

    private String regexRemove(String input, String regex) {
        return regexReplace(input, regex, "");
    }

    private Matcher regex(String input, String regex) {
        Pattern pattern = Pattern.compile(regex);
        return pattern.matcher(input);
    }

    @SuppressWarnings("UnusedReturnValue")
    private String filter(String filter) {
        this.squiggly = SquigglyJackson.builder(filter).build();
        this.squiggly.apply(objectMapper);
        return filter;
    }

    private String stringify() {
        return stringify(issue);
    }

    private String stringify(Object object) {
        return CoreObjectMappers.stringify(objectMapper, object);
    }


    private String stringifyRaw() {
        return stringifyRaw(issue);
    }

    private String stringifyRaw(Object object) {
        return CoreObjectMappers.stringify(rawObjectMapper, object);
    }

    private void fileTest(String inputFile, String filterFile, String expectedFile) {
        String input = readFile(BASE_PATH + "/input/" + inputFile);
        String filter = readFile(BASE_PATH + "/tests/" + filterFile);
        String expected = readFile(BASE_PATH + "/tests/" + expectedFile);

        try {
            Object inputObject = rawObjectMapper.readValue(input, Object.class);
            Object expectedObject = rawObjectMapper.readValue(expected, Object.class);

            filter(sanitizeFilter(filter));
            assertEquals(stringifyRaw(expectedObject), stringify(inputObject));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private String readFile(String path) {
        URL resource = Thread.currentThread().getContextClassLoader().getResource(path);

        if (resource == null) {
            throw new IllegalArgumentException("path " + path + " does not exist");
        }

        try {
            return new String(Files.readAllBytes(Paths.get(resource.toURI())), StandardCharsets.UTF_8);
        } catch (IOException | URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    private String sanitizeFilter(String filter) {
        String[] lines = filter.split("\n");
        StringBuilder builder = new StringBuilder(filter.length());

        for (String line : lines) {
            line = line.trim();

            if (line.startsWith("#")) {
                continue;
            }

            builder.append(line.replaceAll("\\s", ""));
        }

        return builder.toString();
    }

    private static class DashObject {

        private String id;

        @JsonProperty("full-name")
        private String fullName;

        public DashObject() {
        }

        public DashObject(String id, String fullName) {
            this.id = id;
            this.fullName = fullName;
        }

        public String getId() {
            return id;
        }

        public String getFullName() {
            return fullName;
        }
    }
}
