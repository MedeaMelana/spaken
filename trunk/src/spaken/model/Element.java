/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.List;

import spaken.model.rendered.Rendered;

public interface Element<T extends Element> {

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
}
