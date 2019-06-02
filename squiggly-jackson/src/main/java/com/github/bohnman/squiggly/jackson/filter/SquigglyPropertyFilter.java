package com.github.bohnman.squiggly.jackson.filter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.core.util.JsonGeneratorDelegate;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.fasterxml.jackson.databind.ser.std.MapProperty;
import com.github.bohnman.squiggly.match.SquigglyExpressionMatcher;
import com.github.bohnman.squiggly.filter.SquigglyFilterContext;
import com.github.bohnman.squiggly.function.SquigglyFunctionInvoker;
import com.github.bohnman.squiggly.jackson.serialize.SquigglyJacksonSerializer;
import com.github.bohnman.squiggly.node.ExpressionNode;
import com.github.bohnman.squiggly.node.FilterNode;
import com.github.bohnman.squiggly.node.StatementNode;
import com.github.bohnman.squiggly.path.support.DefaultObjectPath;
import com.github.bohnman.squiggly.path.SquigglyObjectPathElement;

import javax.annotation.Nullable;
import javax.annotation.concurrent.ThreadSafe;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.github.bohnman.squiggly.match.SquigglyExpressionMatcher.ALWAYS_MATCH;
import static com.github.bohnman.squiggly.match.SquigglyExpressionMatcher.NEVER_MATCH;


/**
 * A Jackson @{@link com.fasterxml.jackson.databind.ser.PropertyFilter} that filters objects using squiggly syntax.
 * <p>Here are some examples of squiggly syntax:</p>
 * <pre>
 *    // grab the id and name fields
 *    id,name
 *
 *    // grab the id and nested first name and last name from a the user property
 *    id,user{firstName,lastName}
 *
 *    // grab the full object graph
 *    **
 *
 *    // grab just the base fields
 *    base
 *
 *    // grab all fields of the current object, but just the base fields of nested objects
 *    *
 *
 *    // grab fields starting with eco
 *    eco*
 *
 *    // grab fields ending with Time
 *    *Time
 *
 *    // grab fields containing Weight
 *    *Weight*
 *
 *    // grab the firstName field of the nested employee and manager objects
 *    employee{firstName},manager{firstName}
 *    employee|manager{firstName}
 *
 *    // grab all fields annotated with @PropertyView("hardware") or a derived annotation
 *    hardware
 * </pre>
 */
@SuppressWarnings("unchecked")
@ThreadSafe
public class SquigglyPropertyFilter extends SimpleBeanPropertyFilter {
    public static final String FILTER_ID = "squigglyFilter";

    private final SquigglyExpressionMatcher expressionMatcher;
    private final SquigglyFunctionInvoker functionInvoker;
    private final SquigglyJacksonSerializer serializer;

    /**
     * Constructor.
     *
     * @param squiggly squiggly
     */
    public SquigglyPropertyFilter(
            SquigglyExpressionMatcher expressionMatcher,
            SquigglyFunctionInvoker functionInvoker,
            SquigglyJacksonSerializer serializer
    ) {
        this.expressionMatcher = expressionMatcher;
        this.functionInvoker = functionInvoker;
        this.serializer = serializer;
    }

    @Override
    protected boolean include(final BeanPropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @Override
    protected boolean include(final PropertyWriter writer) {
        throw new UnsupportedOperationException("Cannot call include without JsonGenerator");
    }


    @SuppressWarnings("Duplicates")
    @Override
    public void serializeAsField(final Object pojo, JsonGenerator jgen, final SerializerProvider provider,
                                 final PropertyWriter writer) throws Exception {

//        if (jgen instanceof JsonGeneratorWrapper) {
//            System.out.println("WRAPPER");
//        } else {
//            jgen = new JsonGeneratorWrapper(jgen);
//            System.out.println("NO-WRAPPER");
//        }

        ExpressionNode match = match(writer, jgen);

        if (match != null && match != NEVER_MATCH) {
            if (match.getKeyFunctions().isEmpty() && match.getValueFunctions().isEmpty()) {
                serializer.serializeAsIncludedField(pojo, jgen, provider, writer);
            } else if (writer instanceof BeanPropertyWriter) {
                BeanPropertyWriter beanPropertyWriter = (BeanPropertyWriter) writer;
                String name = "" + functionInvoker.invoke(writer.getName(), writer.getName(), pojo, match.getKeyFunctions());
                Object beanValue = beanPropertyWriter.get(pojo);
                Object value = functionInvoker.invoke(beanValue, beanValue, pojo, match.getValueFunctions());
                serializer.serializeAsConvertedField(pojo, jgen, provider, writer, name, value);
            } else if (writer instanceof MapProperty) {
                MapProperty mapProperty = (MapProperty) writer;
                String name = "" + functionInvoker.invoke(writer.getName(), writer.getName(), pojo, match.getKeyFunctions());
                Object value = functionInvoker.invoke(pojo, pojo, pojo, match.getValueFunctions());
                serializer.serializeAsConvertedField(pojo, jgen, provider, writer, name, value);
            } else {
                serializer.serializeAsIncludedField(pojo, jgen, provider, writer);
            }
        } else if (!jgen.canOmitFields()) {
            serializer.serializeAsExcludedField(pojo, jgen, provider, writer);
        }
    }

    private ExpressionNode match(final PropertyWriter writer, final JsonGenerator jgen) {
        JsonStreamContext streamContext = getStreamContext(jgen);

        if (streamContext == null) {
            return ALWAYS_MATCH;
        }

        if (!squiggly.getContextProvider().isFilteringEnabled()) {
            return ALWAYS_MATCH;
        }

        DefaultObjectPath path = getPath(writer, streamContext);
        SquigglyFilterContext context = squiggly.getContextProvider().getContext(path.getFirst().getObjectClass(), squiggly.getFilterRepository(), squiggly.getParser());

        String filter = context.getFilter();
        FilterNode parsedFilter = context.getFilterNode();
        List<StatementNode> statements = parsedFilter.getStatements();

        if (statements.isEmpty()) {
            return NEVER_MATCH;
        }

        if (statements.size() > 1) {
            throw new IllegalArgumentException("Currently, only a single statement is supported for property filters.");
        }

        return expressionMatcher.match(path, filter, statements.get(0));
    }

    private DefaultObjectPath getPath(PropertyWriter writer, JsonStreamContext sc) {
        LinkedList<SquigglyObjectPathElement> elements = new LinkedList<>();

        if (sc != null) {
            elements.add(SquigglyObjectPathElement.create(writer.getName(), sc.getCurrentValue()));
            sc = sc.getParent();
        }

        while (sc != null) {
            if (sc.getCurrentName() != null && sc.getCurrentValue() != null) {
                elements.addFirst(SquigglyObjectPathElement.create(sc.getCurrentName(), sc.getCurrentValue()));
            }
            sc = sc.getParent();
        }

        return new DefaultObjectPath(elements);
    }

    private JsonStreamContext getStreamContext(JsonGenerator jgen) {
        return jgen.getOutputContext();
    }

    private class ExecutionAwareGenerator extends JsonGeneratorDelegate {

        public ExecutionAwareGenerator(JsonGenerator generator) {
            super(generator);
        }
    }

//    public static void main(String[] args) throws IOException {
//
////        Map<String, Object> map = new HashMap<>();
////        map.put("foo", "bar");
//
////        String filter = "name=name.firstName";
////        String filter = "nickNames@filter(::$.name.@equals('rbohn'))";
//        Person person = new Person("Ryan", "Bohn", 38, "rbohn", "bohnman", "doogie");
////        Person person = new Person(new Name("Ryan", "Bohn"));
////        Person person = new Person(null);
////        mapper.writeValue(System.out, Squiggly.builder().build().apply((JsonNode) mapper.valueToTree(person), filter));
//
//        String filter = "**";
//        boolean usePropertyFilter = true;
//        StaticFilterProvider contextProvider = new StaticFilterProvider(filter) {
//            @Override
//            public boolean isFilteringEnabled() {
//                return usePropertyFilter;
//            }
//        };
//        SquigglyJackson squiggly = SquigglyJackson.builder(contextProvider).build();
//        Object input = person;
//        FilterNode filterNode;
//
//        if (usePropertyFilter) {
//            filterNode = squiggly.getParser().parsePropertyFilter(filter);
//        } else {
//            filterNode = squiggly.getParser().parseNodeFilter(filter);
//        }
//
//        PrintStream out = System.out;
//        PrintStream err = System.err;
//
//        out.println("================================================================================");
//        out.println("Parse Tree");
//        out.println("================================================================================");
//        out.println(squiggly.apply(new ObjectMapper()).writeValueAsString(filterNode));
//
//        out.println();
//        out.println("================================================================================");
//        out.println("Result");
//        out.println("================================================================================");
//
//        if (usePropertyFilter) {
//            out.println(squiggly.apply(new ObjectMapper()).writeValueAsString(input));
//        } else {
//            out.println(squiggly.apply(new ObjectMapper()));
//        }
//        out.println();
//    }
//
////    public static class Person {
////        private final Name name;
////
////        public Person(Name name) {
////            this.name = name;
////        }
////
////        public Name getName() {
////            return name;
////        }
////    }
////
////    public static class Name {
////        private String firstName;
////        private String lastName;
////
////        public Name(String firstName, String lastName) {
////            this.firstName = firstName;
////            this.lastName = lastName;
////        }
////
////        public String getFirstName() {
////            return firstName;
////        }
////
////        public String getLastName() {
////            return lastName;
////        }
////    }


    public static class NickName implements Comparable<NickName> {
        private static final AtomicInteger SEQUENCE = new AtomicInteger();
        private final String name;
        private final int priority = SEQUENCE.incrementAndGet();

        public NickName(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        @Override
        public String toString() {
            return name;
        }

        public String getChuck() {
            return "chuckie";
        }

        public int[] getNumbers() {
            return new int[]{1, 5, 9, 14, 9};
        }

        public int getPriority() {
            return priority;
        }

        @Override
        public int compareTo(@Nullable NickName o) {
            return (o == null) ? -1 : name.compareTo(o.name);
        }
    }

    public static class Person {
        private final String firstName;
        private final String lastName;
        private List<NickName> nickNames;
        private NickName firstNickName;
        //        @JsonIgnore
        private int age;

        public Person(String firstName, String lastName, int age, String... nickNames) {
            this.age = age;
            this.firstName = firstName;
            this.lastName = lastName;
            this.nickNames = Arrays.stream(nickNames).map(NickName::new).collect(Collectors.toList());
            this.firstNickName = this.nickNames.isEmpty() ? null : this.nickNames.get(0);
        }

        public int getAge() {
            return age;
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public NickName getFirstNickName() {
            return firstNickName;
        }

        public List<NickName> getNickNames() {
            return nickNames;
        }

        public String getNullProperty() {
            return null;
        }

        public int[] getNumbers() {
            return new int[]{1, 1, 5, 9, 14, 9};
        }


    }
}
