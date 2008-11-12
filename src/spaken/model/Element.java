/* Created on Jun 20, 2008. */
package spaken.model;

import spaken.model.rendered.Rendered;

public interface Element {

	public Rendered render() throws ImaginaryPointException;
	
	public Element[] getDependencies();

}
