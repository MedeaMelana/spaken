package spaken.ui.swing.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.*;

import spaken.storage.FlatFileStorage;
import spaken.ui.swing.SpaceCanvas;

public class HackySaveAction extends AbstractAction {

	private SpaceCanvas canvas;
	private static final String testfile = "/tmp/SpakenTest.txt";
	
	public HackySaveAction(SpaceCanvas canvas) {
		super("Save to " + testfile);
		
		this.canvas = canvas;
	}

	public void actionPerformed(ActionEvent e) {
		try {
			new FlatFileStorage(new File(testfile)).storeSpace(canvas.getSpace());
		} catch (IOException e1) {
			e1.printStackTrace();
		}
	}

}
