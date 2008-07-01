package spaken.ui.swing.actions;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

public class ExitAction extends AbstractAction {

	public ExitAction() {
		super("Exit");
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(
				KeyEvent.VK_Q, ActionEvent.META_MASK));
	}

	public void actionPerformed(ActionEvent e) {
		System.exit(0);
	}

}
