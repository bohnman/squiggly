package com.github.bohnman.squiggly.util.array;

import java.util.Arrays;

public class ArrayWrappers {

    public static ArrayWrapper create(Object array) {
        if (!array.getClass().isArray()) {
            throw new IllegalArgumentException(String.format("Object [%s] is not an array.", array));
        }

        if (array instanceof Object[]) {
            return new ObjectArrayWrapper((Object[]) array);
        }

        if (array instanceof boolean[]) {
            return new BooleanArrayWrapper((boolean[]) array);
        }

        if (array instanceof char[]) {
            return new CharArrayWrapper((char[]) array);
        }

        if (array instanceof byte[]) {
            return new ByteArrayWrapper((byte[]) array);
        }

        if (array instanceof short[]) {
            return new ShortArrayWrapper((short[]) array);
        }

        if (array instanceof int[]) {
            return new IntArrayWrapper((int[]) array);
        }

        if (array instanceof long[]) {
            return new LongArrayWrapper((long[]) array);
        }

        if (array instanceof float[]) {
            return new FloatArrayWrapper((float[]) array);
        }

        if (array instanceof double[]) {
            return new DoubleArrayWrapper((double[]) array);
        }

        throw new IllegalStateException("Unhandled array class: " + array.getClass());
    }


    private static class BooleanArrayWrapper implements ArrayWrapper {
        private final boolean[] array;

        public BooleanArrayWrapper(boolean[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (boolean) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }

    }


    private static class CharArrayWrapper implements ArrayWrapper {
        private final char[] array;

        public CharArrayWrapper(char[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (char) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class ByteArrayWrapper implements ArrayWrapper {
        private final byte[] array;

        public ByteArrayWrapper(byte[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (byte) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class ShortArrayWrapper implements ArrayWrapper {
        private final short[] array;

        public ShortArrayWrapper(short[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (short) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class IntArrayWrapper implements ArrayWrapper {
        private final int[] array;

        public IntArrayWrapper(int[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (int) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }


    private static class LongArrayWrapper implements ArrayWrapper {
        private final long[] array;

        public LongArrayWrapper(long[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (long) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class FloatArrayWrapper implements ArrayWrapper {
        private final float[] array;

        public FloatArrayWrapper(float[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (float) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class DoubleArrayWrapper implements ArrayWrapper {
        private final double[] array;

        public DoubleArrayWrapper(double[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = (double) value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }

    private static class ObjectArrayWrapper implements ArrayWrapper {
        private final Object[] array;

        public ObjectArrayWrapper(Object[] array) {
            this.array = array;
        }

        @Override
        public Object get(int index) {
            return array[index];
        }

        @Override
        public Object set(int index, Object value) {
            Object old = array[index];
            array[index] = value;
            return old;
        }

        @Override
        public int size() {
            return array.length;
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(array);
        }

        @Override
        public String toString() {
            return Arrays.toString(array);
        }

        @Override
        public Object getArray() {
            return array;
        }
    }
}
