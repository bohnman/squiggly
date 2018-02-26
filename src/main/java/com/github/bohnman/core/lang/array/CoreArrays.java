package com.github.bohnman.core.lang.array;

import java.lang.reflect.Array;
import java.util.Arrays;

public class CoreArrays {

    private static final String[] EMPTY_STRING_ARRAY = new String[0];

    private CoreArrays() {
    }

    public static <T> T[] clone(final T[] array) {
        if (array == null) {
            return null;
        }
        return array.clone();
    }

    public static int normalizeIndex(int index, int length) {
        return normalizeIndex(index, length, 0, length);
    }

    public static int normalizeIndex(int index, int length, int min, int max) {
        if (length == 0) {
            return min;
        }

        if (index < min) {
            return Math.max(min, length + index);
        }

        return Math.min(index, max);
    }

    public static String[] emptyStringArray() {
        return EMPTY_STRING_ARRAY;
    }

    @SuppressWarnings("unchecked")
    public static <T> T[] newArray(Class<T> type, int length) {
        return (T[]) Array.newInstance(type, length);
    }

    public static CoreArrayWrapper wrap(Object array) {
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


    private static class BooleanArrayWrapper implements CoreArrayWrapper {
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


    private static class CharArrayWrapper implements CoreArrayWrapper {
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

    private static class ByteArrayWrapper implements CoreArrayWrapper {
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

    private static class ShortArrayWrapper implements CoreArrayWrapper {
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

    private static class IntArrayWrapper implements CoreArrayWrapper {
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


    private static class LongArrayWrapper implements CoreArrayWrapper {
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

    private static class FloatArrayWrapper implements CoreArrayWrapper {
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

    private static class DoubleArrayWrapper implements CoreArrayWrapper {
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

    private static class ObjectArrayWrapper implements CoreArrayWrapper {
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
