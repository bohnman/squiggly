package com.github.bohnman.squiggly.spring.boot.starter.config;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SequenceWriter;
import com.github.bohnman.core.lang.CoreStrings;
import com.github.bohnman.squiggly.core.config.SquigglyConfig;
import com.github.bohnman.squiggly.core.context.provider.SquigglyContextProvider;
import com.github.bohnman.squiggly.core.context.provider.ThreadLocalSquigglyContextProvider;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterCustomizer;
import com.github.bohnman.squiggly.core.filter.SquigglyFilterHolder;
import com.github.bohnman.squiggly.jackson.Squiggly;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.jackson.JacksonAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.ResolvableType;
import org.springframework.http.MediaType;
import org.springframework.http.codec.ServerCodecConfigurer;
import org.springframework.http.codec.json.Jackson2JsonEncoder;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.util.MimeType;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.reactive.config.WebFluxConfigurationSupport;
import org.springframework.web.server.WebFilter;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

@Configuration
@ConditionalOnWebApplication
@ConditionalOnClass(DispatcherHandler.class)
@AutoConfigureAfter(JacksonAutoConfiguration.class)
public class SquigglyWebFluxAutoConfiguration {

    public static final String FILTER_HINT = SquigglyWebFluxAutoConfiguration.class.getName() + ".filter";

    @Autowired(required = false)
    SquigglyFilterCustomizer filterCustomizer;

    @Bean
    public WebFilter squigglyWebFilter(SquigglyConfig config) {
        return (exchange, chain) -> {
            String filter = exchange.getRequest().getQueryParams().getFirst(config.getFilterRequestParam());

            if (CoreStrings.isNotEmpty(filter)) {
                exchange = exchange.mutate().request(exchange.getRequest()
                        .mutate()
                        .header(FILTER_HINT, filter)
                        .build())
                        .build();
            }

            return chain.filter(exchange);
        };
    }


    @Bean
    @ConditionalOnMissingBean
    public SquigglyContextProvider squigglyRequestContextProvider() {
        return new ThreadLocalSquigglyContextProvider() {
            @Nullable
            @Override
            protected String customizeFilter(@Nullable String filter, @Nullable Class beanClass) {
                return (filterCustomizer == null) ? filter : filterCustomizer.apply(filter, beanClass);
            }
        };
    }

    @Bean
    public static SquigglyAutoConfiguration.SquigglyApplicationListener squigglyApplicationListener(Squiggly squiggly) {
        return new RequestSquigglyApplicationListener(squiggly);
    }

    public static class RequestSquigglyApplicationListener extends SquigglyAutoConfiguration.SquigglyApplicationListener {
        public RequestSquigglyApplicationListener(Squiggly squiggly) {
            super(squiggly);
        }
    }


    @SuppressWarnings("NullableProblems")
    @Configuration
    public static class WebFluxConfig extends WebFluxConfigurationSupport {

        @Autowired
        private ObjectMapper objectMapper;


        @Override
        @Bean
        public ServerCodecConfigurer serverCodecConfigurer() {
            ServerCodecConfigurer configurer = super.serverCodecConfigurer();
            configurer.defaultCodecs().jackson2JsonEncoder(new JsonEncoder(objectMapper));
            return configurer;
        }
    }

    private static class JsonEncoder extends Jackson2JsonEncoder {

        public JsonEncoder(ObjectMapper mapper, MimeType... mimeTypes) {
            super(mapper, mimeTypes);
        }

        @Override
        public Map<String, Object> getEncodeHints(ResolvableType actualType, ResolvableType elementType, MediaType mediaType, ServerHttpRequest request, ServerHttpResponse response) {
            Map<String, Object> hints = new HashMap<>(super.getEncodeHints(actualType, elementType, mediaType, request, response));
            hints.put(FILTER_HINT, request.getHeaders().getFirst(FILTER_HINT));
            return hints;
        }

        @Override
        protected ObjectWriter customizeWriter(ObjectWriter writer, MimeType mimeType, ResolvableType elementType, Map<String, Object> hints) {
            String filter = (String) hints.get(FILTER_HINT);

            if (filter == null) {
                return writer;
            }


            return new ObjectWriterWrapper(writer, filter);
        }
    }

    private static class ObjectWriterWrapper extends ObjectWriter {

        private final String filter;

        public ObjectWriterWrapper(ObjectWriter base, String filter) {
            super(base, base.getConfig());
            this.filter = filter;
        }

        @Override
        public void writeValue(JsonGenerator gen, Object value) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                super.writeValue(gen, value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public ObjectWriter withView(Class<?> view) {
            try {
                SquigglyFilterHolder.set(filter);
                return super.withView(view);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValues(File out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValues(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValues(JsonGenerator gen) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValues(gen);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValues(Writer out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValues(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValues(OutputStream out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValues(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(File out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValuesAsArray(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(JsonGenerator gen) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValuesAsArray(gen);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(Writer out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValuesAsArray(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public SequenceWriter writeValuesAsArray(OutputStream out) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValuesAsArray(out);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public void writeValue(File resultFile, Object value) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                super.writeValue(resultFile, value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public void writeValue(OutputStream out, Object value) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                super.writeValue(out, value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public void writeValue(Writer w, Object value) throws IOException {
            try {
                SquigglyFilterHolder.set(filter);
                super.writeValue(w, value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public String writeValueAsString(Object value) throws JsonProcessingException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValueAsString(value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }

        @Override
        public byte[] writeValueAsBytes(Object value) throws JsonProcessingException {
            try {
                SquigglyFilterHolder.set(filter);
                return super.writeValueAsBytes(value);
            } finally {
                SquigglyFilterHolder.remove();
            }
        }
    }
}
