package spaken.util;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

public class ListWithoutDuplicates<E> implements List<E> {
	List<E> innerList;

	Set<E> innerSet;

	public ListWithoutDuplicates() {
		innerList = new LinkedList<E>();
		innerSet = new HashSet<E>();
	}

	public ListWithoutDuplicates(Collection<? extends E> collection) {
		this();
		addAll(collection);
	}

	/**
	 * Adds an element to this list. If the list already contains the element,
	 * the list does not change.
	 * 
	 * @param e
	 *            the element to add.
	 * @return <tt>true</tt> if the list has changed.
	 */
	public boolean add(E e) {
		if (contains(e)) {
			return false;
		} else {
			innerSet.add(e);
			return innerList.add(e);
		}
	}

	/**
	 * Adds an element to this list at a specific index. If the list already
	 * contains the element, the element is moved to the specified index.
	 * 
	 * @param i
	 *            the index for the element.
	 * @param e
	 *            the element to add.
	 */
	public void add(int i, E e) {
		remove(e);
		innerList.add(i, e);
	}

	/**
	 * Adds the first occurance of each elements of another collection to this
	 * list.
	 * 
	 * @param es
	 *            the other collection.
	 * @return <tt>true</tt> if the collection has changed.
	 */
	public boolean addAll(Collection<? extends E> es) {
		boolean changed = false;

		for (E e : es) {
			if (!innerSet.contains(e)) {
				innerSet.add(e);
				innerList.add(e);
				changed = true;
			}
		}
		return changed;
	}

	/**
	 * @throws UnsupportedOperationException.
	 *             Ha!
	 */
	public boolean addAll(int i, Collection<? extends E> es) {
		throw new UnsupportedOperationException("Too complicated.");
	}

	public void clear() {
		innerSet.clear();
		innerList.clear();
	}

	public boolean contains(Object e) {
		return innerSet.contains(e);
	}

	public boolean containsAll(Collection<?> es) {
		return innerSet.containsAll(es);
	}

	public boolean equals(Object that) {
		return innerList.equals(that);
	}

	public E get(int i) {
		return innerList.get(i);
	}

	public int hashCode() {
		return innerList.hashCode();
	}

	public int indexOf(Object e) {
		return innerList.indexOf(e);
	}

	public boolean isEmpty() {
		return innerList.isEmpty();
	}

	/**
	 * @return a read-only iterator over this list.
	 */
	public Iterator<E> iterator() {
		return Collections.unmodifiableList(innerList).iterator();
	}

	/**
	 * indexOf(e) == lastIndexOf(e)
	 */
	public int lastIndexOf(Object e) {
		return innerList.lastIndexOf(e);
	}

	/**
	 * @return a read-only iterator over this list.
	 */
	public ListIterator<E> listIterator() {
		return Collections.unmodifiableList(innerList).listIterator();
	}

	/**
	 * @return a read-only iterator over this list.
	 */
	public ListIterator<E> listIterator(int i) {
		return Collections.unmodifiableList(innerList).listIterator(i);
	}

	public E remove(int i) {
		E e = innerList.get(i);
		innerSet.remove(e);
		return innerList.remove(i);
	}

	public boolean remove(Object e) {
		innerSet.remove(e);
		return innerList.remove(e);
	}

	public boolean removeAll(Collection<?> es) {
		// TODO could be wrong
		innerSet.removeAll(es);
		return innerList.removeAll(es);
	}

	public boolean retainAll(Collection<?> es) {
		// TODO could be wrong
		innerSet.retainAll(es);
		return innerList.retainAll(es);
	}

	/**
	 * Overwrites the specified index with the given element. If this list
	 * already contains the element at another index, that occurrence will be
	 * removed.
	 * 
	 * @param i
	 *            index.
	 * @param e
	 *            element.
	 * @return the old element at index i.
	 */
	public E set(int i, E e) {
		int j = innerList.indexOf(e);
		E old = innerList.set(i, e);
		if (j >= 0 && i != j) {
			innerList.remove(j);
		}
		innerSet.remove(old);
		innerSet.add(e);
		return old;
	}

	public int size() {
		return innerList.size();
	}

	/**
	 * @throws UnsupportedOperationException
	 */
	public List<E> subList(int from, int to) {
		throw new UnsupportedOperationException(
				"We should implement this, but we can't be bothered.");
	}

	public Object[] toArray() {
		return innerList.toArray();
	}

	public <T> T[] toArray(T[] array) {
		return innerList.toArray(array);
	}
	
	@Override
	public String toString() {
		return innerList.toString();
	}

}
