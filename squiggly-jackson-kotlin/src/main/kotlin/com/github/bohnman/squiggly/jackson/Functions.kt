package com.github.bohnman.squiggly.jackson

import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider
import com.github.bohnman.squiggly.core.convert.ConverterRecord
import com.github.bohnman.squiggly.core.function.FunctionExecutionRequest
import com.github.bohnman.squiggly.core.function.SquigglyFunction
import com.github.bohnman.squiggly.core.function.SquigglyParameter
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionClass
import com.github.bohnman.squiggly.core.function.annotation.SquigglyFunctionMethod
import com.github.bohnman.squiggly.jackson.config.SquigglyCustomizer
import kotlin.reflect.KCallable
import kotlin.reflect.KClass
import kotlin.reflect.KType
import kotlin.reflect.KVisibility
import kotlin.reflect.full.companionObject
import kotlin.reflect.full.companionObjectInstance
import kotlin.reflect.full.findAnnotation
import kotlin.reflect.full.valueParameters
import kotlin.reflect.jvm.javaType

// Builders

/**
 * Create Squiggly using a builder and a filter.
 *
 * @param filter the filter string
 */
fun squiggly(filter: String, init: Squiggly.Builder.() -> Unit): Squiggly =
        squigglyBuilder(init).context(SimpleSquigglyContextProvider(filter)).build()

/**
 * Create Squiggly using a builder.
 */
fun squiggly(init: Squiggly.Builder.() -> Unit): Squiggly =
        squigglyBuilder(init).build()

/**
 * Create and return a builder.
 */
fun squigglyBuilder(init: Squiggly.Builder.() -> Unit): Squiggly.Builder {
    val builder = Squiggly.builder()
    builder.init()
    return builder
}

/**
 * Create a customizer using a builder.
 */
fun squigglyCustomizer(init: Squiggly.Builder.() -> Unit): SquigglyCustomizer {
    return squigglyCustomizer(init)
}

// Extensions

/**
 * Register a converter.
 *
 * @param source source class
 * @param target target class
 * @param function converter function
 */
fun <S : Any, T: Any> Squiggly.Builder.converter(source: KClass<S>, target: KClass<T>, function: (S) -> T): Squiggly.Builder {
    return converter(source.java, target.java, function)
}

/**
 * Register a converter.
 *
 * @param function converter function
 */
inline fun <reified S : Any, reified T: Any> Squiggly.Builder.converter(noinline function: (S) -> T): Squiggly.Builder {
    return converter(S::class.java, T::class.java, function)
}

/**
 * Register a converter.
 *
 * @param callable a callable
 */
fun Squiggly.Builder.converter(callable: KCallable<*>): Squiggly.Builder {
    check(callable.valueParameters.size == 1){ "${callable.name} must have a single parameter"}
    check(!callable.valueParameters[0].isVararg) { "${callable.name} is varags, which is not supported"}

    val sourceType = inferJavaClass(callable.valueParameters[0].type)
    val targetType = inferJavaClass(callable.returnType)
    val record = ConverterRecord(sourceType, targetType) { callable.call(it) }

    return converter(record)
}

/**
 * Register a function.
 *
 * @param callable function
 */
fun Squiggly.Builder.function(callable: KCallable<*>): Squiggly.Builder {
    return function(callable, null)
}

/**
 * Register functions.
 *
 * @param callables functions
 */
fun Squiggly.Builder.functions(vararg callables: KCallable<*>): Squiggly.Builder {
    callables.forEach { function(it) }
    return this
}

/**
 * Register a function.
 *
 * @param callable function
 * @param owner owner object
 */
fun Squiggly.Builder.function(callable: KCallable<*>, owner: Any?): Squiggly.Builder {
    return function(CallableSquigglyFunction(callable, owner))
}

/**
 * Register class functions.
 *
 * @param kclass class whose functions will be registered.
 */
fun Squiggly.Builder.function(kclass: KClass<*>): Squiggly.Builder {
    return function(kclass, mutableSetOf())
}

/**
 * Register functions of all classses provided.
 *
 * @param kclasses classes
 */
fun Squiggly.Builder.functions(vararg kclasses: KClass<*>): Squiggly.Builder {
    kclasses.forEach { function(it) }
    return this
}


private fun Squiggly.Builder.function(kclass: KClass<*>, processed: MutableSet<KClass<*>>): Squiggly.Builder {
    if (processed.contains(kclass)) {
        return this
    }

    val classAnnotation = kclass.findAnnotation<SquigglyFunctionClass>()
    val prefix = classAnnotation?.prefix ?: ""
    val registrationStrategy = classAnnotation?.strategy ?: SquigglyFunction.RegistrationStrategy.AUTO

    kclass.companionObject?.members
            ?.filter { it.visibility == KVisibility.PUBLIC }
            ?.filter {
                val method = it.findAnnotation<SquigglyFunctionMethod>()
                method?.ignore?.not() ?: registrationStrategy == SquigglyFunction.RegistrationStrategy.AUTO
            }
            ?.forEach { CallableSquigglyFunction(it, kclass.companionObjectInstance, prefix) }

    classAnnotation?.include?.forEach { function(it, processed) }

    return this
}

private fun inferJavaClass(type: KType) : Class<*> {
    if (type.classifier is KClass<*>) {
        return (type.classifier as KClass<*>).java
    }

    return type.javaType.javaClass
}

private class CallableSquigglyFunction(private val callable: KCallable<*>, private val owner: Any?, private val prefix: String = "") : SquigglyFunction<Any> {
    private val name: String
    private val returnType: Class<*>
    private val parameters: List<SquigglyParameter>
    private val aliases: List<String>

    init {
        val methodAnnotation: SquigglyFunctionMethod? = callable.annotations
                .filterIsInstance(SquigglyFunctionMethod::class.java)
                .firstOrNull()

        name = prefix + (methodAnnotation?.value ?: callable.name)
        aliases = methodAnnotation?.aliases?.map { prefix + it }?.toList() ?: emptyList()
        returnType = inferJavaClass(callable.returnType)

        parameters = callable.valueParameters
                .map {
                    SquigglyParameter.builder(inferJavaClass(it.type))
                            .varArgs(it.isVararg)
                            .build()
                }

    }

    override fun getName(): String {
        return name
    }

    override fun getAliases(): MutableList<String> {
        return aliases.toMutableList()
    }

    override fun getReturnType(): Class<*> {
        return returnType
    }

    override fun getParameters(): MutableList<SquigglyParameter> {
        return parameters.toMutableList()
    }

    override fun apply(req: FunctionExecutionRequest): Any? {
        if (owner == null) {
            return callable.call(*req.parameters.toTypedArray())
        }

        return callable.call(owner, *req.parameters.toTypedArray())
    }
}