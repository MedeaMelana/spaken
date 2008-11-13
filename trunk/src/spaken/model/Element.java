/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.List;

import spaken.model.rendered.Rendered;

public interface Element {

	public Rendered render() throws ImaginaryPointException;
	
	// TODO Already obsolete. Remove if a new use does not emerge quickly :)
	public Element[] getDependencies();

	public void makePluggable(List<PluggablePoint> collect);

}
