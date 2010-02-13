package spaken.util;

import java.util.*;

public class Iterables {
	public static <T> void addAll(Collection<T> collection, Iterable<? extends T> newElements) {
		for (T e : newElements) {
			collection.add(e);
		}
	}
	
	public static <T> Iterable<T> catIterables(final Iterable<? extends T> it1, final Iterable<? extends T> it2) {
		return new Iterable<T>() {

			@SuppressWarnings("unchecked")
			public Iterator<T> iterator() {
				return new CatIterable<T>(new Iterable[] {it1, it2});
			}
			
		};
	}
	
	private static class CatIterable<T> implements Iterator<T> {
		private Iterator<? extends T> current;
		private int index;
		private Iterable<? extends T>[] iterators;

		private CatIterable(Iterable<? extends T>[] its) {
			this.iterators = its;
			this.index = 0;
			this.current = its[0].iterator();
		}
		
		public boolean hasNext() {
			while (current != null) {
				if (current.hasNext()) return true;
				index++;
				if (index < iterators.length) {
					current = iterators[index].iterator();
				} else {
					current = null;
				}
			}
			return false;
		}

		public T next() {
			if (hasNext()) {
				return current.next();
			} else {
				throw new NoSuchElementException();
			}
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
	
}
