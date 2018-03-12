package com.github.bohnman.squiggly.jackson

import com.github.bohnman.squiggly.core.context.provider.SimpleSquigglyContextProvider
import com.github.bohnman.squiggly.core.convert.ConverterRecord
import com.github.bohnman.squiggly.core.function.FunctionRequest
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

fun squiggly(filter: String, init: Squiggly.Builder.() -> Unit): Squiggly =
        squigglyBuilder(init).context(SimpleSquigglyContextProvider(filter)).build()

fun squiggly(init: Squiggly.Builder.() -> Unit): Squiggly =
        squigglyBuilder(init).build()

fun squigglyBuilder(init: Squiggly.Builder.() -> Unit): Squiggly.Builder {
    val builder = Squiggly.builder()
    builder.init()
    return builder
}

fun squigglyCustomer(init: Squiggly.Builder.() -> Unit): SquigglyCustomizer {
    return squigglyCustomer(init)
}

// Extensions

fun <S : Any, T: Any> Squiggly.Builder.converter(source: KClass<S>, target: KClass<T>, function: (S) -> T): Squiggly.Builder {
    return converter(source.java, target.java, function)
}

inline fun <reified S : Any, reified T: Any> Squiggly.Builder.converter(noinline function: (S) -> T): Squiggly.Builder {
    return converter(S::class.java, T::class.java, function)
}

fun Squiggly.Builder.converter(callable: KCallable<*>): Squiggly.Builder {
    check(callable.valueParameters.size == 1){ "${callable.name} must have a single parameter"}
    check(!callable.valueParameters[0].isVararg, { "${callable.name} is varags, which is not supported"})

    val sourceType = inferJavaClass(callable.valueParameters[0].type)
    val targetType = inferJavaClass(callable.returnType)
    val record = ConverterRecord(sourceType, targetType) { callable.call(it) }

    return converter(record)
}


fun Squiggly.Builder.function(callable: KCallable<*>): Squiggly.Builder {
    return function(callable, null)
}

fun Squiggly.Builder.functions(vararg callables: KCallable<*>): Squiggly.Builder {
    callables.forEach { function(it) }
    return this
}

fun Squiggly.Builder.function(callable: KCallable<*>, owner: Any?): Squiggly.Builder {
    return function(CallableSquigglyFunction(callable, owner))
}

fun Squiggly.Builder.function(kclass: KClass<*>): Squiggly.Builder {
    return function(kclass, mutableSetOf())
}

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

    override fun apply(req: FunctionRequest): Any? {
        if (owner == null) {
            return callable.call(*req.parameters.toTypedArray())
        }

        return callable.call(owner, *req.parameters.toTypedArray())
    }
}