/*
 * Copyright 2012-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sample.data.jpa;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.datatype.hibernate5.Hibernate5Module;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.web.RequestSquigglyContextProvider;
import com.github.bohnman.squiggly.web.SquigglyRequestFilter;
import org.hibernate.collection.spi.PersistentCollection;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.domain.Page;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import javax.servlet.http.HttpServletRequest;

@SpringBootApplication
public class SampleDataJpaApplication {

    @Bean
    public FilterRegistrationBean squigglyRequestFilter() {
        FilterRegistrationBean filter = new FilterRegistrationBean();
        filter.setFilter(new SquigglyRequestFilter());
        filter.setOrder(1);
        return filter;
    }

    public static void main(String[] args) throws Exception {
        ConfigurableApplicationContext context = SpringApplication.run(SampleDataJpaApplication.class, args);

        Iterable<ObjectMapper> objectMappers = context.getBeansOfType(ObjectMapper.class)
                .values();

        objectMappers.forEach(mapper -> mapper.registerModule(new Hibernate5Module()));

        Squiggly.init(objectMappers, new RequestSquigglyContextProvider() {
            @Override
            public void serializeAsIncludedField(Object pojo, JsonGenerator jgen, SerializerProvider provider, PropertyWriter writer) throws Exception {
                if (isFilteringEnabled()) {
                    Object value = writer.getMember().getValue(pojo);

                    if (value instanceof PersistentCollection) {
                        ((PersistentCollection) value).forceInitialization();
                    }
                }

                super.serializeAsIncludedField(pojo, jgen, provider, writer);
            }

            @Override
            protected String customizeFilter(String filter, HttpServletRequest request, Class<?> beanClass) {

                if (filter != null && Page.class.isAssignableFrom(beanClass)) {
                    filter = "**,content[" + filter + "]";
                }

                return filter;
            }
        });


        ObjectMapper objectMapper = Iterables.getFirst(objectMappers, null);

        // Enable Squiggly for Jackson message converter
        if (objectMapper != null) {
            for (MappingJackson2HttpMessageConverter converter : context.getBeansOfType(MappingJackson2HttpMessageConverter.class).values()) {
                converter.setObjectMapper(objectMapper);
            }
        }
    }

}
