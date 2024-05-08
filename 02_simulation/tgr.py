# this is the base package for triple gamma regularizazion
import torch
import torch.nn as nn
import torch.optim as optim

import log_hyperu as hyperu

# Triple-Gamma-Regularization

class TripleGammaRegularization(nn.Module):
    def __init__(self, num_features):
        super(TripleGammaRegularization, self).__init__()
        self.linear = nn.Linear(4, 1)

    def forward(self, x):
        return self.linear(x)

def TripleGammaReg_Loss(Y_HAT, Y, coefficients, lamda, a, c, kappa, norm):
    # Compute Phi value for third parameter input of the hypergeometric function
    phi = (2*c)/((kappa**2)*a)
    
    # Compute the squared loss ||y - X*beta||^2_2 (Using the variables: ||Y - Y_HAT||^2_2
    squared_loss = nn.MSELoss()(Y_HAT, Y)
    
    # Compute the penalty term using Triple Gamma Regularization
    if(norm == True):
        penalty_term = torch.sum(-hyperu.log_hyperu(torch.tensor([[c+0.5]]),torch.tensor([[1.5-a]]),(coefficients**2)/(2*phi))+hyperu.log_hyperu(torch.tensor([[c+0.5]]),torch.tensor([[1.5-a]]),torch.tensor([[0.0]])))
    else:
        penalty_term = torch.sum(-hyperu.log_hyperu(torch.tensor([[c+0.5]]),torch.tensor([[1.5-a]]),(coefficients**2)/(2*phi)))


    # Copmute the sum of the squared loss and the penalty term
    total_loss = squared_loss + lamda*penalty_term
    
    return total_loss

def TripleGammaModel(X, y, penalty, a, c, kappa,  norm=True, num_epochs=1000, lr=0.01):
    model = TripleGammaRegularization(X.shape[1])
    optimizer = optim.SGD(model.parameters(), lr=lr)
    coef_list = list()
    loss_list = list()

    for epoch in range(num_epochs):
        optimizer.zero_grad()
        outputs = model(X)
        loss = TripleGammaReg_Loss(outputs, y, model.linear.weight, penalty, a, c, kappa, norm)
        loss.backward()
        coef_list.append(model.linear.weight)
        loss_list.append(loss)
        
        #Gradient Clipping???
        nn.utils.clip_grad_norm_(model.parameters(), 1.0)
        optimizer.step()

        if (epoch+1) % 100 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {loss.item()}')

    return model, coef_list, loss_list
