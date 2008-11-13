/* Created on Jun 20, 2008. */
package spaken.model;

import java.io.IOException;
import java.util.List;

import spaken.model.elements.PluggablePoint;
import spaken.model.rendered.Rendered;
import spaken.storage.*;

public interface Element<T extends Element> {
	/*
	 * TODO Elements cannot currently safely implement an equals method, because
	 * FlatFileStorage assumes, by using a HashMap, that equals does the same as
	 * ==. Fix this first if you want equals.
	 */

	/*
	 * Ideetje: misschien is makePluggableCopy wel uit te drukken in
	 * writeElement met een hippe magische ElementWriter.
	 */

	public Rendered render() throws ImaginaryPointException;

	// TODO Already obsolete. Remove if a new use does not emerge quickly :)
	public Element[] getDependencies();

	/**
	 * Make a deep copy of this <tt>Element</tt>, down to <tt>FixedPoint</tt>s,
	 * replacing those by <tt>PluggablePoint</tt>s. While doing this, collect
	 * all generated <tt>PluggablePoint</tt>s in the specified list.
	 * 
	 * @param collect
	 *            the list in which to collect <tt>PluggablePoint</tt>s.
	 * @return a deep copy of this <tt>Element</tt>.
	 */
	public T makePluggableCopy(List<PluggablePoint> collect);

	/**
	 * Write this <tt>Element</tt> using the specified <tt>ElementWriter</tt>.
	 * Call this method whenever you like.
	 * 
	 * @param out
	 *            The <tt>ElementWriter</tt> that should be filled with data.
	 * @throws IOException
	 */
	public void writeElement(ElementWriter out) throws IOException;

	/**
	 * Restore this <tt>Element</tt> using the specified
	 * <tt>ElementReader</tt>. This method is only to be called by
	 * <tt>StorageEngine</tt>s, and only on newly created instances. It is
	 * extremely inpolite to call this method from elsewhere.
	 * 
	 * TODO Think about alternative: require a constructor that takes an
	 * ElementReader. This is more reflection-heavy and cannot be required by
	 * the interface.
	 * 
	 * @param in
	 *            The <tt>ElementReader</tt> that contains the data for
	 *            restoration.
	 * @throws IOException
	 */
	public void readElement(ElementReader in) throws IOException;

}
