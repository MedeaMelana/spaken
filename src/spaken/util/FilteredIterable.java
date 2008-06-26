package spaken.util;

import java.util.Iterator;

public class FilteredIterable<T,B> implements Iterable<B> {

	private Iterable<? extends T> value;
	private Filter<? super T,B> filter;

	public FilteredIterable(Iterable<? extends T> value,
			Filter<? super T,B> filter) {
		this.value = value;
		this.filter = filter;
	}

	public Iterator<B> iterator() {
		return new FilteredIterator(value.iterator());
	}

	class FilteredIterator implements Iterator<B> {

		private Iterator<? extends T> iterator;

		private B next;

		public FilteredIterator(Iterator<? extends T> iterator) {
			this.iterator = iterator;
			fetchNext();
		}

		public boolean hasNext() {
			return next != null;
		}

		public B next() {
			B b = next;
			fetchNext();
			return b;
		}

		private void fetchNext() {
			while (iterator.hasNext()) {
				T nxt = iterator.next();
				if (filter.accepts(nxt)) {
				  next = filter.map(nxt);
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
