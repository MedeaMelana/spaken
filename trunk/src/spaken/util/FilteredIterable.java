package spaken.util;

import java.util.Iterator;

public class FilteredIterable<T> implements Iterable<T> {

	private Iterable<? extends T> value;
	private Filter<? super T> filter;

	public FilteredIterable(Iterable<? extends T> value,
			Filter<? super T> filter) {
		this.value = value;
		this.filter = filter;
	}

	public Iterator<T> iterator() {
		return new FilteredIterator(value.iterator());
	}

	class FilteredIterator implements Iterator<T> {

		private Iterator<? extends T> iterator;

		private T next;

		public FilteredIterator(Iterator<? extends T> iterator) {
			this.iterator = iterator;
			fetchNext();
		}

		public boolean hasNext() {
			return next != null;
		}

		public T next() {
			T t = next;
			fetchNext();
			return t;
		}

		private void fetchNext() {
			while (iterator.hasNext()) {
				next = iterator.next();
				if (filter.accepts(next)) {
					return;
				}
			}
			next = null;
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

}
